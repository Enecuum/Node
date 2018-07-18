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

data MicroBlockContent
data MacroblockBD
data HashOfMicroblock
data To
data From
type Number = Integer
data HashOfKeyBlock


data MsgToDB where
    KeyBlockMsgToDB :: Value -> MsgToDB
    MicroblockMsgToDB :: Microblock -> MsgToDB

    MyTail :: MVar Number -> MsgToDB
    GetHashOfLastClosedKeyBlock :: MVar (Maybe (HashOfKeyBlock, Number)) -> MsgToDB
    PeekNPreviousClosedKeyBlocks :: Int -> HashOfKeyBlock -> MVar (Maybe [(HashOfKeyBlock, Number)]) -> MsgToDB

    GetKeyBlockSproutData :: From -> To -> MVar (Maybe [MacroblockBD])-> MsgToDB
    SetKeyBlockSproutData :: [MacroblockBD] -> MVar Bool -> MsgToDB

    GetRestSproutData :: [HashOfMicroblock] -> MVar (Maybe MicroBlockContent) -> MsgToDB
    SetRestSproutData :: MicroBlockContent -> MVar Bool -> MsgToDB

    SproutFullyTransfered :: MsgToDB

startDBActor descriptor aMicroblockCh aValueChan aInfoCh (aInChan, aOutChan) = do
    void . C.forkIO . forever . writeInChan aInChan . MicroblockMsgToDB =<< readChan aMicroblockCh
    void . C.forkIO . forever . writeInChan aInChan . KeyBlockMsgToDB =<< readChan aValueChan
    forever $ readChan aOutChan >>= \case
        MicroblockMsgToDB aMicroblock ->
            addMicroblockToDB descriptor aMicroblock aInfoCh

        KeyBlockMsgToDB aValue ->
            addMacroblockToDB descriptor aValue aInfoCh

        MyTail aMVar -> do
            putMVar aMVar =<< myTail descriptor aInfoCh

        GetHashOfLastClosedKeyBlock aMVar -> do
            aRes <- try $ getHashOfLastClosedKeyBlock descriptor aInfoCh
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        PeekNPreviousClosedKeyBlocks aInt aHashOfKeyBlock aMVar -> do
            aRes <- try $ peekNPreviousClosedKeyBlocks descriptor aInfoCh aInt aHashOfKeyBlock
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            aRes <- try $ getKeyBlockSproutData descriptor aInfoCh aFrom aTo
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            aIsValid <- isValidKeyBlockSprout descriptor aInfoCh aMacroblockBD
            when aIsValid $ setKeyBlockSproutData descriptor aInfoCh aMacroblockBD
            putMVar aMVar aIsValid

        GetRestSproutData aMickroBlockHash aMVar -> do
            aRes <- try $ getRestSproutData descriptor aInfoCh aMickroBlockHash
            case aRes of
                Right aJustRes              -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException)   -> putMVar aMVar Nothing

        SetRestSproutData aMicroBlockContent aMVar -> do
            aIsValid <- isValidRestOfSprout descriptor aInfoCh aMicroBlockContent
            when aIsValid $ setRestSproutData descriptor aInfoCh aMicroBlockContent
            putMVar aMVar aIsValid

        SproutFullyTransfered -> sproutFullyTransfered descriptor aInfoCh


myTail :: DBPoolDescriptor -> InChan InfoMsg -> IO Number
myTail = undefined

getHashOfLastClosedKeyBlock ::  DBPoolDescriptor -> InChan InfoMsg -> IO (HashOfKeyBlock, Number)
getHashOfLastClosedKeyBlock = undefined

peekNPreviousClosedKeyBlocks :: DBPoolDescriptor -> InChan InfoMsg -> Int -> HashOfKeyBlock -> IO [(HashOfKeyBlock, Number)]
peekNPreviousClosedKeyBlocks = undefined

setKeyBlockSproutData :: DBPoolDescriptor -> InChan InfoMsg -> [MacroblockBD] -> IO ()
setKeyBlockSproutData = undefined

getKeyBlockSproutData :: DBPoolDescriptor -> InChan InfoMsg -> From -> To -> IO [MacroblockBD]
getKeyBlockSproutData = undefined

getRestSproutData :: DBPoolDescriptor -> InChan InfoMsg -> [HashOfMicroblock] -> IO MicroBlockContent
getRestSproutData = undefined

setRestSproutData :: DBPoolDescriptor -> InChan InfoMsg -> MicroBlockContent  -> IO ()
setRestSproutData = undefined

isValidKeyBlockSprout :: DBPoolDescriptor -> InChan InfoMsg -> [MacroblockBD] -> IO Bool
isValidKeyBlockSprout = undefined

isValidRestOfSprout :: DBPoolDescriptor -> InChan InfoMsg -> MicroBlockContent -> IO Bool
isValidRestOfSprout = undefined

sproutFullyTransfered :: DBPoolDescriptor -> InChan InfoMsg -> IO ()
sproutFullyTransfered = undefined
