{-# LANGUAGE LambdaCase #-}
module Node.SyncServer where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Monad
import           Node.DBActor
import           Data.Aeson                            as A
import           Lens.Micro
import           Node.Data.GlobalLoging
import           Node.Data.Key
import           Node.Node.Types
import           Node.NetLvl.Massages
import           Service.Chan
import           Service.InfoMsg                       (InfoMsg)
import           Service.InfoMsg
import           Service.Sync.SyncJson
import           Service.Transaction.SproutCommon
import           Service.Types
import           Service.Timer


syncServer :: (InChan SyncEvent, OutChan SyncEvent) -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO b
syncServer (aInputSync, outSyncChan) aDBActorChan aManagerChan aInfoChan = do
    let aLog aMsg = writeLog aInfoChan [SyncTag] Info aMsg
    writeLog aInfoChan [SyncTag, InitTag] Info "Init syncServer"
    metronome 60000000 $ writeInChan aInputSync RestartSync
    forever $ do
      aLog $ "Start of syncServer stap."
      readChan outSyncChan >>= \case
        RestartSync -> do
            aLog "Request of all tails"
            aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
            forM_ aActualConnects $ \(ActualConnectInfo aNodeId aNodeType _) -> do
                when (aNodeType == NN) $ sendMsgToNode aManagerChan RequestTail aNodeId

        SyncMsg aNodeId aMsg -> do
            aLog $ "Recived SyncMsg to syncServer: " ++ show aMsg
            let aSend amsg = sendMsgToNode aManagerChan amsg aNodeId
            case aMsg of
                RequestTail                                 -> do
                    aLog "Processing of RequestTail."
                    takeRecords aDBActorChan MyTail >>= \case
                        Just aTail  -> do
                            aLog $ "aTail " ++ show aTail
                            aSend $ ResponseTail $ fst aTail
                        Nothing     -> do
                            aLog "Empty tail #001"
                            aSend $ StatusSyncMessage "Empty tail" "#001"

                ResponseTail aNum -> do
                    aLog $ "Recived response tail " ++ show aNum
                    aMyTail  <- takeMyTail aDBActorChan
                    aLog $ "My tail is: " ++ show aMyTail
                    when (aMyTail < aNum) $ do
                        aLog $ "Sending of chain request."
                        aSend RequestChain
                ResponseChain aChunk -> do
                    aLog $ "Response chain" ++ show aChunk
                    writeInChan aDBActorChan (WriteChain aChunk)

                RequestChain ->
                    aSend . ResponseChain =<< takeRecords aDBActorChan GetChain

                        {-
                        lastCommonNumber <- findBeforeFork aMyTail aNodeId outSyncChan aDBActorChan aManagerChan aInfoChan
                        void $ loadBlocks outSyncChan aDBActorChan aManagerChan (lastCommonNumber+1) aNum aNodeId aInfoChan
                        -}
{-
                PeekKeyBlokRequest aFrom aTo                -> do
                    aLog "Processing of PeekKeyBlokRequest."
                    takeRecords aDBActorChan (GetKeyBlockSproutData aFrom aTo) >>=
                        aSend . PeekKeyBlokResponse

                MicroblockRequest aHash         -> do
                    aLog "Processing of MicroblockRequest."
                    takeRecords aDBActorChan (GetRestSproutData aHash) >>= \case
                      Just mblock -> aSend $ MicroblockResponse mblock
                      Nothing     -> aSend $ StatusSyncMessage ("No block with this hash " ++ show aHash ++ " in DB")  "#003"

                PeekHashKblokRequest aFrom aTo -> do
                    aLog "Processing of PeekHashKblokRequest."
                    takeRecords aDBActorChan (PeekNKeyBlocks aFrom aTo) >>=
                        aSend . PeekHashKblokResponse

-}
                _ -> aSend $ StatusSyncMessage ("Send command are not allowed for server")  "#005"


data Response a = Response NodeId a deriving Show

{-
takePeekKeyBlokResponse     :: OutChan SyncEvent -> InChan InfoMsg -> IO (Response [(Number, HashOfKeyBlock, MacroblockBD)])
takePeekKeyBlokResponse aChan aLog = do
    readChan aChan >>= \case
        SyncMsg aId aMsg  -> case aMsg of
            PeekKeyBlokResponse aList  -> return $ Response aId aList
            StatusSyncMessage _ "#002" -> return $ Response aId []
            _                          -> takePeekKeyBlokResponse aChan aLog
        _                 -> takePeekKeyBlokResponse aChan aLog

takeMicroblockResponse      :: OutChan SyncEvent -> IO (Response (Maybe MicroBlockContent))
takeMicroblockResponse aChan = readChan aChan >>= \case
    SyncMsg aId aMsg  -> case aMsg of

      MicroblockResponse aContent -> return $ Response aId (Just aContent)
      StatusSyncMessage _ "#003"  -> return $ Response aId Nothing
      _                           -> takeMicroblockResponse aChan
    _                 -> takeMicroblockResponse aChan


takePeekHashKblokResponse   :: OutChan SyncEvent -> IO (Response [(Number, HashOfKeyBlock)])
takePeekHashKblokResponse aChan = readChan aChan >>= \case
    SyncMsg aId aMsg  -> case aMsg of
        PeekHashKblokResponse aList -> return $ Response aId aList
        StatusSyncMessage _ "#004"  -> return $ Response aId []
        _                           -> takePeekHashKblokResponse aChan
    _                 -> takePeekHashKblokResponse aChan

-}
--
{-
loadOneMacroBlock
    ::  OutChan SyncEvent
    ->  InChan MsgToDB
    ->  InChan MsgToCentralActor
    ->  [HashOfMicroblock] -- Microblock for loading
    ->  NodeId
    ->  InChan InfoMsg
    ->  HashOfKeyBlock
    ->  Number
    ->  IO Bool
loadOneMacroBlock outSyncChan aDBActorChan aManagerChan (x:xs) aNodeId aInfoChan hashOfKeyBlock aNumber = do
    writeLog aInfoChan [SyncTag] Info $ "Loading of microblocks. From " ++ show aNodeId
    sendMsgToNode aManagerChan (MicroblockRequest x) aNodeId
    Response _ aMicroBlockContent <- takeMicroblockResponse outSyncChan
    writeLog aInfoChan [SyncTag] Info $ "Microblock loaded. From " ++ show aNodeId
    case aMicroBlockContent of
        Just aJustMicroBlockContent -> do
            writeLog aInfoChan [SyncTag] Info $ "Seting of microblocs:" ++ show aJustMicroBlockContent
            _ <- takeRecords aDBActorChan (SetRestSproutData (aNumber, hashOfKeyBlock,  aJustMicroBlockContent))
            writeLog aInfoChan [SyncTag] Info $ "Microbloc seted:" ++ show aJustMicroBlockContent
            loadOneMacroBlock outSyncChan aDBActorChan aManagerChan xs aNodeId aInfoChan hashOfKeyBlock aNumber
        Nothing -> return False
loadOneMacroBlock _ _ _ _ _ _ _ _ = return True
-}
{-

-- load kblocks
loadBlocks :: OutChan SyncEvent -> InChan MsgToDB -> InChan MsgToCentralActor -> From -> To -> NodeId -> InChan InfoMsg -> IO Bool
loadBlocks outSyncChan aDBActorChan aManagerChan aFrom aTo aId aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Loading of blocks: process start. From " ++ show aId ++ ". (" ++ show aFrom ++ ", " ++ show aTo ++ ")"
    sendMsgToNode aManagerChan (PeekKeyBlokRequest aFrom aTo) aId
    let aTake = do
            Response aNodeId aList <- takePeekKeyBlokResponse outSyncChan aInfoChan
            if aNodeId == aId then return aList else aTake
    aListOfBlocks <- aTake
    writeLog aInfoChan [SyncTag] Info " Kblocks recieved."
    aOk <- takeRecords aDBActorChan (SetKeyBlockSproutData aListOfBlocks)
    writeLog aInfoChan [SyncTag] Info " Seting of kblocks"
    if not aOk then do
        writeLog aInfoChan [SyncTag] Info "DeleteSproutData"
        writeInChan aDBActorChan $ DeleteSproutData aFrom
        return False
    else do
        writeLog aInfoChan [SyncTag] Info "Loading of macrblock"
        loadMacroBlocks outSyncChan aDBActorChan aManagerChan aFrom aListOfBlocks aId aInfoChan

-}
{-
-- load microblocks
loadMacroBlocks
    ::  OutChan SyncEvent
    ->  InChan MsgToDB
    ->  InChan MsgToCentralActor
    ->  Number -- number of block for delete (old chain)
    ->  [(Number, HashOfKeyBlock, MacroblockBD)]
    ->  NodeId
    ->  InChan InfoMsg
    ->  IO Bool
loadMacroBlocks a1 a2 a3 aNumber (x:xs) aId aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Loading of macrblock. From " ++ show aId ++ "Block: " ++ show  x
    aOk <- loadOneMacroBlock a1 a2 a3 ((third x)^.mblocks) aId aInfoChan (second x) (first x)
    writeLog aInfoChan [SyncTag] Info "Macrblock loaded?"
    if aOk then loadMacroBlocks a1 a2 a3 aNumber xs aId aInfoChan
    else do
        writeLog aInfoChan [SyncTag] Info $ "Delete sprout " ++ show aNumber
        writeInChan a2 $ DeleteSproutData aNumber
        return False

loadMacroBlocks _ a2 _ aNumber _ _ aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Set sprout as main " ++ show aNumber
    writeInChan a2 $ SetSproutAsMain aNumber
    return True
-}

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c


--

-- find length of my chain
takeMyTail :: InChan MsgToDB -> IO Number
takeMyTail aDBActorChan =
    takeRecords aDBActorChan MyTail >>= \case
        Just (aNum, _)  -> return aNum
        Nothing         -> return 0


takeTailNum :: Response (Number, a) -> Number
takeTailNum (Response _ (aNum, _)) = aNum

{-
-- find number of last common kblock
findBeforeFork :: Number -> NodeId -> OutChan SyncEvent -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO Number
findBeforeFork 0 _ _ _ _ aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Find before fork: " ++ show 0
    return 0

findBeforeFork n aId outSyncChan aDBActorChan aManagerChan aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Find before fork: " ++ show n
    sendMsgToNode aManagerChan (PeekHashKblokRequest n n) aId
    let aTakePeek aIdThis aChan = do
            Response remoteNodeId aRes <- takePeekHashKblokResponse aChan
            if aIdThis == remoteNodeId then return aRes else aTakePeek aIdThis aChan
    [(_, aHash)]   <- aTakePeek aId outSyncChan
    [(_, aMyHash)] <- takeRecords aDBActorChan (PeekNKeyBlocks n n)

    if aHash == aMyHash
    then return n
    else findBeforeFork (n-1) aId outSyncChan aDBActorChan aManagerChan aInfoChan
-}
--
takeRecords :: InChan a -> (MVar p -> a) -> IO p
takeRecords aChan aTaker  = do
    aVar <- newEmptyMVar
    writeInChan aChan $ aTaker aVar
    takeMVar aVar

--
sendMsgToNode :: ToJSON a => InChan MsgToCentralActor -> a -> NodeId -> IO ()
sendMsgToNode aChan aMsg aId = writeInChan aChan $ SendSyncMsg aId (toJSON aMsg)
