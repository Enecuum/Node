{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Node.DBActor where

import Control.Exception
import Service.InfoMsg
import Node.Data.GlobalLoging
import Control.Concurrent.Chan.Unagi.Bounded
import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Data.Aeson

import qualified Data.ByteString as B
import Control.Monad
import Service.Types
import Service.Chan
import Service.Transaction.LedgerSync
import Service.Transaction.SproutCommon
import Service.Sync.SyncJson
import Service.Transaction.Common (
        DBPoolDescriptor (..)
    ,   addMacroblockToDB
    ,   addMicroblockToDB
  )


data MsgToDB where
    KeyBlockMsgToDB       :: Value -> MsgToDB
    MicroblockMsgToDB     :: Microblock -> MsgToDB

    MyTail                :: MVar (Maybe (Number, HashOfKeyBlock)) -> MsgToDB
    PeekNKeyBlocks        :: From -> To -> MVar [(Number, HashOfKeyBlock)] -> MsgToDB
    GetKeyBlockSproutData :: From -> To -> MVar [(Number, HashOfKeyBlock, MacroblockBD)]-> MsgToDB
    SetKeyBlockSproutData :: [(Number, HashOfKeyBlock, MacroblockBD)] -> MVar Bool -> MsgToDB
    GetRestSproutData     :: HashOfMicroblock -> MVar (Maybe MicroBlockContent) -> MsgToDB
    SetRestSproutData     :: (Number, HashOfKeyBlock, MicroBlockContent) -> MVar Bool -> MsgToDB
    DeleteSproutData      :: Number -> MsgToDB
    SetSproutAsMain       :: Number -> MsgToDB


startDBActor
    ::  DBPoolDescriptor
    ->  OutChan Microblock
    ->  OutChan Value
    ->  InChan InfoMsg
    ->  (InChan MsgToDB, OutChan MsgToDB)
    ->  IO b
startDBActor descriptor aMicroblockCh aValueChan aInfoCh (aInChan, aOutChan) = do
    writeLog aInfoCh [BDTag, InitTag] Info "Init. Starting of DBActor."
    void . C.forkIO $ do
        writeLog aInfoCh [BDTag, InitTag] Info "Init. Resender of microblocs."
        forever $ do
            val <- readChan aMicroblockCh
            writeInChan aInChan $ MicroblockMsgToDB val

    void . C.forkIO $ do
        writeLog aInfoCh [BDTag, InitTag] Info "Init. Resender of KeyBlocks."
        forever $ do
            val <- readChan aValueChan
            writeInChan aInChan $ KeyBlockMsgToDB val

    let aData = Common descriptor aInfoCh
    writeLog aInfoCh [BDTag, InitTag] Info "Init. DBActor started."
    forever $ readChan aOutChan >>= \case
        MicroblockMsgToDB aMicroblock -> do
            writeLog aInfoCh [BDTag] Info "Recived mickrobloc."
            addMicroblockToDB descriptor aMicroblock aInfoCh

        KeyBlockMsgToDB aValue -> do
            writeLog aInfoCh [BDTag] Info "Recived keyBlocks."
            addMacroblockToDB descriptor aValue aInfoCh

        MyTail aMVar -> do
            writeLog aInfoCh [BDTag] Info "My tail request."
            aRes <- try $ myTail aData
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        PeekNKeyBlocks aInt aHashOfKeyBlock aMVar -> do
            writeLog aInfoCh [BDTag] Info "Peek NKey blocks request."
            peekNPreviousKeyBlocks aData aInt aHashOfKeyBlock >>= putMVar aMVar

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            writeLog aInfoCh [BDTag] Info "Get key block sprout data request."
            getKeyBlockSproutData aData aFrom aTo >>= putMVar aMVar


        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            writeLog aInfoCh [BDTag] Info "Set key block sprout data request."
            aIsValid <- forM aMacroblockBD (isValidKeyBlockSprout aData.toPair2)
            when (and aIsValid) $ setKeyBlockSproutData aData $ toPair2 <$> aMacroblockBD
            putMVar aMVar (and aIsValid)

        GetRestSproutData aMickroBlockHash aMVar -> do
            writeLog aInfoCh [BDTag] Info "Get rest sprout data request."
            aRes <- try $ getRestSproutData aData aMickroBlockHash
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        SetRestSproutData aMicroBlockContent@(_,_,a) aMVar -> do
            writeLog aInfoCh [BDTag] Info "Set rest sprout data request."
            aIsValid <- isValidRestOfSprout aData a
            when aIsValid $ setRestSproutData aData aMicroBlockContent
            putMVar aMVar aIsValid



        DeleteSproutData arg -> do
            writeLog aInfoCh [BDTag] Info "Delete sprout data request."
            deleteSproutData aData arg

        SetSproutAsMain arg -> do
            writeLog aInfoCh [BDTag] Info  "Set sprout as main request."
            setSproutAsMain aData arg


--
toPair1 :: (a, b, c) -> (a, b)
toPair1 (a, b, _) = (a, b)

toPair2 :: (a, b, c) -> (b, c)
toPair2 (_, b, c) = (b, c)
