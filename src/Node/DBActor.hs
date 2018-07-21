{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Node.DBActor where

import Control.Exception
import Service.InfoMsg
import Control.Concurrent.Chan.Unagi.Bounded
import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Data.Aeson

import qualified Data.ByteString as B
import Control.Monad
import Service.Types (Microblock, MacroblockBD)
import Service.Chan
import Service.Sync.SyncJson
import Service.Transaction.LedgerSync hiding (myTail, HashOfKeyBlock)
import Service.Transaction.Common (
        DBPoolDescriptor (..)
    ,   addMacroblockToDB
    ,   addMicroblockToDB
  )

data CommonData = CommonData {
    db       :: DBPoolDescriptor,
    infoChan :: InChan InfoMsg
  }


data TransactionInfo
data MicroblockBD

myTail ::  CommonData -> IO (Number, HashOfKeyBlock)
myTail = undefined

peekNPreviousKeyBlocks :: CommonData -> From -> To -> IO [(Number, HashOfKeyBlock)]
peekNPreviousKeyBlocks = undefined

getKeyBlockSproutData :: CommonData -> From -> To -> IO [(Number, HashOfKeyBlock, MacroblockBD)]
getKeyBlockSproutData = undefined

isValidKeyBlockSprout :: CommonData -> (HashOfKeyBlock, MacroblockBD) -> IO Bool
isValidKeyBlockSprout = undefined

setKeyBlockSproutData :: CommonData -> [(Number, HashOfKeyBlock, MacroblockBD)] -> IO ()
setKeyBlockSproutData = undefined

getRestSproutData :: CommonData -> HashOfMicroblock -> IO MicroBlockContent
getRestSproutData = undefined

isValidRestOfSprout :: CommonData -> MicroBlockContent -> IO Bool
isValidRestOfSprout = undefined

setRestSproutData :: CommonData -> MicroBlockContent -> IO ()
setRestSproutData = undefined

deleteSproutData      :: CommonData -> Number -> IO ()
deleteSproutData = undefined

setSproutAsMain       :: CommonData -> Number -> IO ()
setSproutAsMain = undefined


data MsgToDB where
    KeyBlockMsgToDB       :: Value -> MsgToDB
    MicroblockMsgToDB     :: Microblock -> MsgToDB

    MyTail                :: MVar (Maybe (Number, HashOfKeyBlock)) -> MsgToDB
    PeekNKeyBlocks        :: From -> To -> MVar [(Number, HashOfKeyBlock)] -> MsgToDB
    GetKeyBlockSproutData :: From -> To -> MVar [(Number, HashOfKeyBlock, MacroblockBD)]-> MsgToDB
    SetKeyBlockSproutData :: [(Number, HashOfKeyBlock, MacroblockBD)] -> MVar Bool -> MsgToDB
    GetRestSproutData     :: HashOfMicroblock -> MVar (Maybe MicroBlockContent) -> MsgToDB
    SetRestSproutData     :: MicroBlockContent -> MVar Bool -> MsgToDB
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
    void . C.forkIO . forever . writeInChan aInChan . MicroblockMsgToDB =<< readChan aMicroblockCh
    void . C.forkIO . forever . writeInChan aInChan . KeyBlockMsgToDB =<< readChan aValueChan
    let aData = CommonData descriptor aInfoCh
    forever $ readChan aOutChan >>= \case
        MicroblockMsgToDB aMicroblock ->
            addMicroblockToDB descriptor aMicroblock aInfoCh

        KeyBlockMsgToDB aValue ->
            addMacroblockToDB descriptor aValue aInfoCh

        MyTail aMVar -> do
            aRes <- try $ myTail aData
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        PeekNKeyBlocks aInt aHashOfKeyBlock aMVar -> do
            peekNPreviousKeyBlocks aData aInt aHashOfKeyBlock >>= putMVar aMVar

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            getKeyBlockSproutData aData aFrom aTo >>= putMVar aMVar


        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            aIsValid <- forM aMacroblockBD (isValidKeyBlockSprout aData.toPair2)
            when (and aIsValid) $ setKeyBlockSproutData aData aMacroblockBD
            putMVar aMVar (and aIsValid)

        GetRestSproutData aMickroBlockHash aMVar -> do
            aRes <- try $ getRestSproutData aData aMickroBlockHash
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        SetRestSproutData aMicroBlockContent aMVar -> do
            aIsValid <- isValidRestOfSprout aData aMicroBlockContent
            when aIsValid $ setRestSproutData aData aMicroBlockContent
            putMVar aMVar aIsValid

        DeleteSproutData arg -> deleteSproutData aData arg
        SetSproutAsMain arg -> setSproutAsMain aData arg


--
toPair1 :: (a, b, c) -> (a, b)
toPair1 (a, b, _) = (a, b)

toPair2 :: (a, b, c) -> (b, c)
toPair2 (_, b, c) = (b, c)
