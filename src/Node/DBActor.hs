{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Node.DBActor where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.Aeson
import           Data.List.Extra
import           Node.Data.GlobalLoging
import           Service.InfoMsg

import           Control.Monad
-- import qualified Data.ByteString                       as B
import           Service.Chan
import           Service.Transaction.Balance (addKeyBlockToDB2)
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
    WriteChain            :: [Chunk] -> MsgToDB


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
        aLog aMsg = writeLog aInfoCh [BDTag] Info aMsg
    writeLog aInfoCh [BDTag, InitTag] Info "Init. DBActor started."
    forever $ readChan aOutChan >>= \case
        MicroblockMsgToDB aMicroblock -> do
            aLog "Recived mickrobloc."
            aExeption <- try $ addMicroblockToDB aData aMicroblock
            case aExeption of
                Right _ -> aLog "Success of setting microblock"
                Left (e :: SomeException) -> aLog $ "Setting false !!! =" ++ show e

        KeyBlockMsgToDB aValue -> do
            writeLog aInfoCh [BDTag] Info "Recived keyBlocks."
            aExeption <- try $ addKeyBlockToDB aData aValue aSyncChan
            case aExeption of
                Right _ -> aLog "Success of setting keyBlock"
                Left (e :: SomeException) -> aLog $ "Setting false !!! =" ++ show e

        WriteChain aChain -> do
            aLog "Recived chain."
            aExeption <- try $ cleanDB aData
            case aExeption of
                Right _ -> aLog "DB cleaned. Ok."
                Left (e :: SomeException) -> aLog $ "Error of db cleaning: " ++ show e
            forM_ (sortOn (\(Chunk a _) -> _number (a :: KeyBlockInfoPoW)) aChain) $ \(Chunk aKeyBlo aMicroblocks) -> do
                aExeption <- try $ addKeyBlockToDB2 aData aKeyBlo aSyncChan
                case aExeption of
                    Right _ -> aLog "KeyBlock loaded ok."
                    Left (e :: SomeException) -> aLog $ "Error of KeyBlock loading: " ++ show e
                forM_ aMicroblocks $ \aBlock -> do
                    aExeption <- try $ addMicroblockToDB aData aBlock
                    case aExeption of
                        Right _ -> aLog "Block loaded ok."
                        Left (e :: SomeException) -> aLog $ "Error of KeyBlock loading: " ++ show e

        MyTail aMVar -> do
            aLog "My tail request."
            aRes <- try $ myTail aData
            case aRes of
                Right aJustRes            -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException) -> putMVar aMVar Nothing

        PeekNKeyBlocks aInt aHashOfKeyBlock aMVar -> do
            aLog "Peek NKey blocks request."
            aRes <- try $ peekNPreviousKeyBlocks aData aInt aHashOfKeyBlock
            case aRes of
                Right aJustRes              -> putMVar aMVar aJustRes
                Left (_ :: SomeException)   -> putMVar aMVar []

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            aLog "Get key block sprout data request."
            aRes <- try $ getKeyBlockSproutData aData aFrom aTo
            case aRes of
                Right aJustRes              -> putMVar aMVar aJustRes
                Left (_ :: SomeException)   -> putMVar aMVar []


        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            aLog "Set key block sprout data request."
            aLog $ "Setting a blocks: " ++ show aMacroblockBD
            _aIsValid <- try $ isValidKeyBlockSprout aData aMacroblockBD
            aIsValid<- case _aIsValid of
                Right aBool -> return aBool
                Left (e :: SomeException) -> do
                    aLog $ "Validation false: " ++ show e
                    return False

            when aIsValid $ do
                aExeption <- try $ setKeyBlockSproutData aData $ toPair2 <$> aMacroblockBD
                case aExeption of
                    Right _ -> aLog "Success of setting"
                    Left (e :: SomeException) -> aLog $ "Setting false !!! =" ++ show e
            putMVar aMVar aIsValid

        GetRestSproutData aMickroBlockHash aMVar -> do
            aLog "Get rest sprout data request."
            aRes <- try $ getRestSproutData aData aMickroBlockHash
            case aRes of
                Right aJustRes            -> putMVar aMVar (Just aJustRes)
                Left (_ :: SomeException) -> putMVar aMVar Nothing

        SetRestSproutData aMicroBlockContent@(_,_,a) aMVar -> do
            aLog "Set rest sprout data request."
            aIsValid <- isValidRestOfSprout aData a
            when aIsValid $ do
                aExeption <- try $ setRestSproutData aData aMicroBlockContent
                case aExeption of
                    Right _ -> aLog "Success of setting"
                    Left (e :: SomeException) -> aLog $ "Setting false !!! =" ++ show e
            putMVar aMVar aIsValid



        DeleteSproutData arg -> do
            aLog "Delete sprout data request."
            aExeption <- try $ deleteSproutData aData arg
            case aExeption of
                Right _ -> aLog "Success of deleting"
                Left (e :: SomeException) -> writeLog aInfoCh [BDTag] Info $ "Deleting false !!! =" ++ show e

        SetSproutAsMain arg -> do
            aLog "Set sprout as main request."
            aExeption <- try $ setSproutAsMain aData arg
            case aExeption of
                Right _ -> aLog "Success of Set sprout as main"
                Left (e :: SomeException) -> aLog $ "Set sprout as main false !!! =" ++ show e



--
toPair1 :: (a, b, c) -> (a, b)
toPair1 (a, b, _) = (a, b)

toPair2 :: (a, b, c) -> (b, c)
toPair2 (_, b, c) = (b, c)
