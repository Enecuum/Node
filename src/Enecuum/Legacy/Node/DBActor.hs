{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Enecuum.Legacy.Node.DBActor where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.List.Extra
import           Data.Maybe
import           Enecuum.Legacy.Node.Data.GlobalLoging
import           Enecuum.Legacy.Service.Chan
import           Enecuum.Legacy.Service.Sync.SyncJson
import           Enecuum.Legacy.Service.Sync.SyncTypes
import           Enecuum.Legacy.Service.Transaction.Common
import           Enecuum.Legacy.Service.Transaction.LedgerSync
import           Enecuum.Legacy.Service.Types


data MsgToDB where
    KeyBlockMsgToDB       :: Value -> MsgToDB
    MicroblockMsgToDB     :: Microblock -> MsgToDB

    MyTail                :: MVar (Maybe (Number, HashOfKeyBlock)) -> MsgToDB
    PeekNKeyBlocks        :: From -> To -> MVar [(Number, HashOfKeyBlock)] -> MsgToDB
    GetKeyBlockSproutData :: From -> To -> MVar [(Number, HashOfKeyBlock, KeyBlockContent)]-> MsgToDB
    SetKeyBlockSproutData :: [(Number, KeyBlockContent)] -> MVar Bool -> MsgToDB
    GetRestSproutData     :: HashOfMicroblock -> MVar (Maybe MicroBlockContent) -> MsgToDB
    SetRestSproutData     :: (Number, HashOfKeyBlock, MicroBlockContent) -> MVar Bool -> MsgToDB
    DeleteSproutData      :: Number -> MsgToDB
    SetSproutAsMain       :: Number -> MsgToDB

    WriteChain            :: [Chunk] -> MsgToDB
    GetChain              :: MVar [Chunk] -> MsgToDB


startDBActor :: DBPoolDescriptor
                      -> OutChan Microblock
                      -> OutChan Value
                      -> InChan InfoMsg
                      -> (InChan MsgToDB, OutChan MsgToDB)
                      -> (InChan SyncEvent, OutChan SyncEvent)
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
            aLog "Received microblock."
            aExeption <- try $ addMicroblockToDB aData aMicroblock Main
            case aExeption of
                Right _ -> aLog "Success of setting microblock"
                Left (e :: SomeException) -> aLog $ "Setting false !!! =" ++ show e

        KeyBlockMsgToDB aValue -> do
            writeLog aInfoCh [BDTag] Info "Received keyBlocks."
            aExeption <- try $ addKeyBlockToDB aData aValue aSyncChan
            case aExeption of
                Right _ -> aLog "Success of setting keyBlock"
                Left (e :: SomeException) -> aLog $ "Setting false !!! =" ++ show e

        WriteChain aChain -> do
            aLog "Received chain."
            aRes <- try $ myTail aData
            aJustTail <- return $ case aRes of
                Right aJustRes            -> fst aJustRes
                Left (_ :: SomeException) -> 0
            if fromEnum aJustTail < length aChain then do
                aExeption <- try $ cleanDB aData
                case aExeption of
                    Right _ -> aLog "DB cleaned. Ok."
                    Left (e :: SomeException) -> aLog $ "Error of db cleaning: " ++ show e
                forM_ (sortOn (\(Chunk a _) -> _number (a :: KeyBlockInfoPoW)) aChain) $ \(Chunk aKeyBlock aMicroblocks) -> do
                    bExeption <- try $ addKeyBlockToDB2 aData aKeyBlock aSyncChan
                    case bExeption of
                        Right _ -> aLog "KeyBlock loaded ok."
                        Left (e :: SomeException) -> aLog $ "Error of KeyBlock loading: " ++ show e
                    forM_ aMicroblocks $ \aBlock -> do
                        cExeption <- try $ addMicroblockToDB aData aBlock Main
                        case cExeption of
                            Right _ -> aLog "Block loaded ok."
                            Left (e :: SomeException) -> aLog $ "Error of KeyBlock loading: " ++ show e
            else return ()

        GetChain aVar -> do
            aLog "Received chain."
            aTail   <- try $ myTail aData
            -- getting number of key bloks.
            aNumber <- case aTail of
                Right (aNum, _) -> return aNum
                Left (e :: SomeException) -> do
                    aLog $ "Error of myTail: " ++ show e
                    return 0

            aChain  <- forM [1..fromEnum aNumber] $ \aNum -> do
                aMicroblocks <- try $ getMicroblocks aData (toInteger aNum)
                aKeyBlock    <- try $ getKeyBlock aData (toInteger aNum)
                case (aKeyBlock, aMicroblocks) of
                  (Right rKeyBlock, Right rMicroblocks) -> return $ Just $ Chunk rKeyBlock rMicroblocks
                  (Left (_ :: SomeException), Left (_ :: SomeException))    -> return Nothing
                  (_, Left (_ :: SomeException))                            -> return Nothing
                  (Left (_ :: SomeException), _)                            -> return Nothing
            putMVar aVar $ catMaybes aChain

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
                Right aJustRes            -> putMVar aMVar aJustRes
                Left (_ :: SomeException) -> putMVar aMVar []

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            aLog "Get key block sprout data request."
            aRes <- try $ getKeyBlockSproutData aData aFrom aTo
            case aRes of
                Right aJustRes            -> putMVar aMVar aJustRes
                Left (_ :: SomeException) -> putMVar aMVar []


        SetKeyBlockSproutData aKeyBlockContent aMVar -> do
            aLog "Set key block sprout data request."
            aLog $ "Setting a blocks: " ++ show aKeyBlockContent
            _aIsValid <- try $ isValidKeyBlockSprout aData aKeyBlockContent
            aIsValid<- case _aIsValid of
                Right aBool -> return aBool
                Left (e :: SomeException) -> do
                    aLog $ "Validation false: " ++ show e
                    return False

            when aIsValid $ do
                let kBlocks = map (\(_, (KeyBlockContent k _)) -> k) $ aKeyBlockContent
                aExeption <- try $ setKeyBlockSproutData aData aSyncChan kBlocks
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
