{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.DBActor where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Aeson
import           Node.Data.GlobalLoging
import           Service.InfoMsg

import           Control.Monad
-- import qualified Data.ByteString                       as B
import           Service.Chan
import           Service.Sync.SyncJson
import           Service.Transaction.Common            (addKeyBlockToDB,
                                                        addMicroblockToDB)
import           Service.Transaction.LedgerSync
import           Service.Transaction.SproutCommon
import           Service.Types


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



startDBActor :: DBPoolDescriptor
                      -> OutChan Microblock
                      -> OutChan Value
                      -> InChan InfoMsg
                      -> (InChan MsgToDB, OutChan MsgToDB)
                      -> (InChan Service.Sync.SyncJson.SyncEvent, b1)
                      -> IO b2
startDBActor descriptor aMicroblockCh aValueChan aInfoCh (aInChan, aOutChan) aSyncChan = do
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
            aExeption <- try $ addMicroblockToDB descriptor aMicroblock aInfoCh
            case aExeption of
                Right _ -> writeLog aInfoCh [BDTag] Info "Success of setting microblock"
                Left (e :: SomeException) -> writeLog aInfoCh [BDTag] Info $ "Setting false !!! =" ++ show e

        KeyBlockMsgToDB aValue -> do
            writeLog aInfoCh [BDTag] Info "Recived keyBlocks."
            aExeption <- try $ addKeyBlockToDB descriptor aValue aInfoCh aSyncChan
            case aExeption of
                Right _ -> writeLog aInfoCh [BDTag] Info "Success of setting keyBlock"
                Left (e :: SomeException) -> writeLog aInfoCh [BDTag] Info $ "Setting false !!! =" ++ show e

        MyTail aMVar -> do
            writeLog aInfoCh [BDTag] Info "My tail request."
            aRes <- try $ myTail aData
            case aRes of
                Right aJustRes            -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException) -> putMVar aMVar Nothing

        PeekNKeyBlocks aInt aHashOfKeyBlock aMVar -> do
            writeLog aInfoCh [BDTag] Info "Peek NKey blocks request."
            peekNPreviousKeyBlocks aData aInt aHashOfKeyBlock >>= putMVar aMVar

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            writeLog aInfoCh [BDTag] Info "Get key block sprout data request."
            getKeyBlockSproutData aData aFrom aTo >>= putMVar aMVar


        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            writeLog aInfoCh [BDTag] Info "Set key block sprout data request."
            writeLog aInfoCh [BDTag] Info $ "Setting a blocks: " ++ show aMacroblockBD
            aIsValid <- isValidKeyBlockSprout aData aMacroblockBD
            when aIsValid $ do
                aExeption <- try $ setKeyBlockSproutData aData $ toPair2 <$> aMacroblockBD
                case aExeption of
                    Right _ -> writeLog aInfoCh [BDTag] Info "Success of setting"
                    Left (e :: SomeException) -> writeLog aInfoCh [BDTag] Info $ "Setting false !!! =" ++ show e
            putMVar aMVar aIsValid

        GetRestSproutData aMickroBlockHash aMVar -> do
            writeLog aInfoCh [BDTag] Info "Get rest sprout data request."
            aRes <- try $ getRestSproutData aData aMickroBlockHash
            case aRes of
                Right aJustRes            -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException) -> putMVar aMVar Nothing

        SetRestSproutData aMicroBlockContent@(_,_,a) aMVar -> do
            writeLog aInfoCh [BDTag] Info "Set rest sprout data request."
            aIsValid <- isValidRestOfSprout aData a
            when aIsValid $ do
                aExeption <- try $ setRestSproutData aData aMicroBlockContent
                case aExeption of
                    Right _ -> writeLog aInfoCh [BDTag] Info "Success of setting"
                    Left (e :: SomeException) -> writeLog aInfoCh [BDTag] Info $ "Setting false !!! =" ++ show e
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
