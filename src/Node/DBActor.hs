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
import Control.Monad
import Service.Types (Microblock)
import Service.Chan
import Service.Transaction.Common (
        DBPoolDescriptor (..)
    ,   addMacroblockToDB
    ,   addMicroblockToDB
  )


data MacroblockBD
data TransactionInfo
data MicroblockBD

data HashOfMicroblock
data HashOfKeyBlock
data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type Number = Integer
type From = Number
type To = Number
data CommonData = CommonData {
  db       :: DBPoolDescriptor,
  infoChan :: InChan InfoMsg
}

myTail ::  CommonData -> IO (Number, HashOfKeyBlock)
myTail = undefined

peekNKeyBlocks :: CommonData -> From -> To -> IO [(Number, HashOfKeyBlock)]
peekNKeyBlocks = undefined

getKeyBlockSproutData :: CommonData -> From -> To -> IO [(Number, HashOfKeyBlock, MacroblockBD)]
getKeyBlockSproutData = undefined

isValidKeyBlockSprout :: CommonData -> (HashOfKeyBlock,MacroblockBD) -> IO Bool
isValidKeyBlockSprout = undefined

setKeyBlockSproutData :: CommonData -> [(HashOfKeyBlock,MacroblockBD)] -> IO ()
setKeyBlockSproutData = undefined

getRestSproutData :: CommonData -> HashOfMicroblock -> IO MicroBlockContent
getRestSproutData = undefined

isValidRestOfSprout :: CommonData -> MicroBlockContent -> IO Bool
isValidRestOfSprout = undefined

setRestSproutData :: CommonData -> MicroBlockContent -> IO ()
setRestSproutData = undefined

deleteSproutData      :: CommonData -> (Number, HashOfKeyBlock) -> IO ()
deleteSproutData = undefined


data MsgToDB where
    KeyBlockMsgToDB       :: Value -> MsgToDB
    MicroblockMsgToDB     :: Microblock -> MsgToDB

    MyTail                :: MVar (Maybe (Number, HashOfKeyBlock)) -> MsgToDB
    PeekNKeyBlocks        :: From -> To -> MVar (Maybe [(Number, HashOfKeyBlock)]) -> MsgToDB
    GetKeyBlockSproutData :: From -> To -> MVar (Maybe [(Number, HashOfKeyBlock, MacroblockBD)])-> MsgToDB
    SetKeyBlockSproutData :: [(HashOfKeyBlock, MacroblockBD)] -> MVar Bool -> MsgToDB
    GetRestSproutData     :: HashOfMicroblock -> MVar (Maybe MicroBlockContent) -> MsgToDB
    SetRestSproutData     :: MicroBlockContent -> MVar Bool -> MsgToDB
    DeleteSproutData      :: (Number, HashOfKeyBlock) -> MsgToDB


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
            aRes <- try $ peekNKeyBlocks aData aInt aHashOfKeyBlock
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            aRes <- try $ getKeyBlockSproutData aData aFrom aTo
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            aIsValid <- forM aMacroblockBD (isValidKeyBlockSprout aData)
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
