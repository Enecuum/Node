
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Lib where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.List.Extra
import           Node.DBActor
import           PoA.Pending

import           Control.Concurrent.Async
import           Data.Aeson                            as A
import qualified Data.ByteString.Lazy                  as L
import           Data.IORef
import           Data.Maybe
-- import qualified Data.Text                             as T
import           Lens.Micro
import           Network.Socket                        (tupleToHostAddress)
import qualified Network.WebSockets                    as WS
import           Node.Data.GlobalLoging
import           Node.Data.Key
import           Node.FileDB.FileServer
import           Node.Node.Config.Make
import           Node.Node.Types
import           PoA.PoAServer
import           PoA.Types
import           Service.Chan
import           Service.InfoMsg                       (InfoMsg)
import           Service.InfoMsg
import           Service.Network.Base
import           Service.Network.WebSockets.Client
import           Service.Sync.SyncJson
import           Service.Transaction.Common            (DBPoolDescriptor (..),
                                                        addMacroblockToDB,
                                                        addMicroblockToDB)
import           Service.Transaction.LedgerSync
import           Service.Transaction.SproutCommon
import           Service.Types                         (MacroblockBD,
                                                        Microblock, Transaction)
import           Service.Types
import           System.Directory                      (createDirectoryIfMissing)
import           System.Environment


startNode
  :: DBPoolDescriptor
     -> BuildConfig
     -> InChan InfoMsg
     -> (InChan SyncEvent
         -> (InChan MsgToCentralActor, OutChan MsgToCentralActor)
         -> IORef NetworkNodeData
         -> IO ())
     -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor)
         -> OutChan (Transaction, MVar Bool)
         -> InChan Microblock
         -> MyNodeId
         -> InChan FileActorRequest
         -> IO a)
     -> IO (InChan MsgToCentralActor, OutChan MsgToCentralActor)
startNode descrDB buildConf infoCh manager startDo = do

    --tmp
    createDirectoryIfMissing False "data"


    managerChan@(aIn, _)                        <- newChan 128
    (aMicroblockChan, outMicroblockChan)        <- newChan 128
    (aValueChan, aOutValueChan)                 <- newChan 128
    (aTransactionChan, outTransactionChan)      <- newChan 128
    (aInFileRequestChan, aOutFileRequestChan)   <- newChan 128
    aPendingChan@(inChanPending, _)             <- newChan 128
    aSyncChan@(aInputSync, outChanSync)         <- newChan 128
    aDBActorChan                                <- newChan 128

    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    (snbc, poa_p, stat_h, stat_p, logs_h, logs_p, log_id) <- getConfigParameters (config^.myNodeId) buildConf aIn

    md  <- newIORef $ makeNetworkData config infoCh aInFileRequestChan aMicroblockChan aTransactionChan aValueChan
    void . C.forkIO $ pendingActor aPendingChan aMicroblockChan outTransactionChan infoCh
    void . C.forkIO $ servePoA (config^.myNodeId) poa_p aIn infoCh aInFileRequestChan aMicroblockChan inChanPending
    void . C.forkIO $ startFileServer aOutFileRequestChan
    void . C.forkIO $ startDBActor descrDB outMicroblockChan aOutValueChan infoCh aDBActorChan aSyncChan
    void . C.forkIO $ manager aInputSync managerChan md
    void $ startDo managerChan outTransactionChan aMicroblockChan (config^.myNodeId) aInFileRequestChan

    let MyNodeId aId = config^.myNodeId

    void $ C.forkIO $ connectManager aSyncChan aDBActorChan aIn (poaPort buildConf) bnList aInFileRequestChan (NodeId aId) inChanPending infoCh
    return managerChan


sec = 1000000

connectManager
    ::  (InChan SyncEvent, OutChan SyncEvent)
    ->  (InChan MsgToDB, b1)
    ->  InChan MsgToCentralActor
    ->  PortNumber
    ->  [Connect]
    ->  InChan FileActorRequest
    ->  NodeId
    ->  InChan PendingAction
    ->  InChan InfoMsg
    ->  IO b2

connectManager aSyncChan (inDBActorChan, _) aManagerChan aPortNumber aBNList aConnectsChan aMyNodeId inChanPending aInfoChan = do
    writeLog aInfoChan [ConnectingTag, InitTag] Info "Manager of connecting started."
    forM_ aBNList $ \(Connect aBNIp aBNPort) -> do
        void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
            WS.sendTextData aConnect . encode $ ActionAddToConnectList aPortNumber
    aConnectLoop aBNList True
  where
    aRequestOfPotencialConnects = \case -- IDEA: add random to BN list
        (Connect aBNIp aBNPort):aTailOfList -> do
            aPotencialConnectNumber <- takeRecords aConnectsChan NumberConnects
            when (aPotencialConnectNumber == 0) $ do
                void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
                    WS.sendTextData aConnect $ encode $ RequestPotentialConnects False
                    aMsg <- WS.receiveData aConnect
                    let ResponsePotentialConnects aConnects = fromJust $ decode aMsg
                    writeInChan aConnectsChan $ AddToFile aConnects
                C.threadDelay sec
                aRequestOfPotencialConnects (aTailOfList ++ [Connect aBNIp aBNPort])
        _       -> return ()

    aConnectLoop aBNList isFirst = do
        aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
        if null aActualConnects then do
            aNumberOfConnects <- takeRecords aConnectsChan NumberConnects
            when (aNumberOfConnects == 0) $ aRequestOfPotencialConnects aBNList
            aConnects <- takeRecords aConnectsChan ReadRecordsFromFile
            forM_ aConnects (connectToNN aConnectsChan aMyNodeId inChanPending aInfoChan aManagerChan)
            C.threadDelay $ 2 * sec

            when isFirst $ void $ C.forkIO $ do
                syncServer aSyncChan inDBActorChan aManagerChan aInfoChan
                writeInChan (fst aSyncChan) RestartSync

            C.threadDelay $ 2*sec
            aConnectLoop aBNList False
        else do
            C.threadDelay $ 10 * sec
            aConnectLoop aBNList False


-- найти длинну своей цепочки
takeMyTail :: InChan MsgToDB -> IO Number
takeMyTail aDBActorChan =
    takeRecords aDBActorChan MyTail >>= \case
        Just (aNum, _)  -> return aNum
        Nothing         -> return 0


takeTailNum :: Response (Number, a) -> Number
takeTailNum (Response _ (aNum, _)) = aNum


-- нойти номер последнего общего блока
findBeforeFork :: Number -> NodeId -> OutChan SyncEvent -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO Number
findBeforeFork 0 _ _ _ _ _ = return 0
findBeforeFork n aId outSyncChan aDBActorChan aManagerChan aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Find before fork: " ++ show n
    sendMsgToNode aManagerChan (PeekHashKblokRequest n n) aId
    let aTakePeek aId aChan = do
            Response remoteNodeId aRes <- takePeekHashKblokResponse aChan
            if aId == remoteNodeId then return aRes else aTakePeek aId aChan
    [(_, aHash)]   <- aTakePeek aId outSyncChan
    [(_, aMyHash)] <- takeRecords aDBActorChan (PeekNKeyBlocks n n)

    if aHash == aMyHash
    then return n
    else findBeforeFork (n-1) aId outSyncChan aDBActorChan aManagerChan aInfoChan


-- загрузить длинны всех цепочек у соседей
requestOfAllTails :: OutChan SyncEvent -> InChan MsgToCentralActor -> IO [Response (Number, Maybe HashOfKeyBlock)]
requestOfAllTails outSyncChan aManagerChan = do
    aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
    forM_ aActualConnects $ \(ActualConnectInfo aNodeId aNodeType _) ->
        when (aNodeType == NN) $ sendMsgToNode aManagerChan RequestTail aNodeId

    -- FIXME: If somebody doesn't answer!!!
    forM aActualConnects $ \_ -> takeResponseTail outSyncChan


-- загрузить микроблоки
loadMacroBlocks
    ::  OutChan SyncEvent
    ->  InChan MsgToDB
    ->  InChan MsgToCentralActor
    ->  Number -- блок для удаления (старая цепочка)
    ->  [(Number, HashOfKeyBlock, MacroblockBD)]
    ->  NodeId
    ->  InChan InfoMsg
    ->  IO Bool
loadMacroBlocks a1 a2 a3 aNumber (x:xs) aId aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Loading of macrblock. From " ++ show aId ++ "Block: " ++ show x
    aOk <- loadOneMacroBlock a1 a2 a3 ((third x)^.mblocks) aId aInfoChan (second x) (first x)
    if aOk then loadMacroBlocks a1 a2 a3 aNumber xs aId aInfoChan
    else do
        writeLog aInfoChan [SyncTag] Info $ "Delete sprout " ++ show aNumber
        writeInChan a2 $ DeleteSproutData aNumber
        return False

loadMacroBlocks a1 a2 a3 aNumber _ aId aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Set sprout as main " ++ show aNumber
    writeInChan a2 $ SetSproutAsMain aNumber
    return True

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

loadOneMacroBlock
    ::  OutChan SyncEvent
    ->  InChan MsgToDB
    ->  InChan MsgToCentralActor
    ->  [HashOfMicroblock] -- Блок для загрузки
    ->  NodeId
    ->  InChan InfoMsg
    ->  HashOfKeyBlock
    ->  Number
    ->  IO Bool
loadOneMacroBlock outSyncChan aDBActorChan aManagerChan (x:xs) aNodeId aInfoChan hashOfKeyBlock aNumber = do
    writeLog aInfoChan [SyncTag] Info $ "Loading of microblocks. From " ++ show aNodeId
    sendMsgToNode aManagerChan (MicroblockRequest x) aNodeId
    Response aId aMicroBlockContent <- takeMicroblockResponse outSyncChan
    case aMicroBlockContent of
        Just aJustMicroBlockContent -> do
            takeRecords aDBActorChan (SetRestSproutData (aNumber, hashOfKeyBlock,  aJustMicroBlockContent))
        Nothing -> return False

loadOneMacroBlock _ _ _ _ _ _ _ _ = return True


-- загрузить блоки
loadBlocks :: OutChan SyncEvent -> InChan MsgToDB -> InChan MsgToCentralActor -> [(Number, HashOfKeyBlock, MacroblockBD)] -> From -> To -> NodeId -> InChan InfoMsg -> IO Bool
loadBlocks outSyncChan aDBActorChan aManagerChan [aHash] aFrom aTo aId aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $ "Loading of blocks: process start. From " ++ show aId ++ ". (" ++ show aFrom ++ ", " ++ show aTo ++ ")"
    sendMsgToNode aManagerChan (PeekKeyBlokRequest aFrom aTo) aId
    let aTake = do
            Response aNodeId aList <- takePeekKeyBlokResponse outSyncChan
            if aNodeId /= aId then return aList else aTake
    aListOfBlocks <- aTake
    aOk <- takeRecords aDBActorChan (SetKeyBlockSproutData aListOfBlocks)
    if not aOk then do
        writeInChan aDBActorChan $ DeleteSproutData (first aHash)
        return False
    else do
        loadMacroBlocks outSyncChan aDBActorChan aManagerChan (first aHash) aListOfBlocks aId aInfoChan

loadBlocks _ _ _ _ _ _ _ _= return False




-- синхронизация
syncProcess :: OutChan SyncEvent -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO ()
syncProcess outSyncChan aDBActorChan aManagerChan aInfoChan = do
    writeLog aInfoChan [SyncTag, InitTag] Info "Init. Process of syncronization."
    allTails <- requestOfAllTails outSyncChan aManagerChan
    let maxTails = reverse $ sortOn takeTailNum allTails
    syncNeighbors outSyncChan aDBActorChan aManagerChan maxTails aInfoChan


-- пройти по списку соседей
syncNeighbors :: OutChan SyncEvent -> InChan MsgToDB -> InChan MsgToCentralActor -> [Response (Number, Maybe HashOfKeyBlock)] -> InChan InfoMsg -> IO ()
syncNeighbors outSyncChan aDBActorChan aManagerChan (x:xs) aInfoChan = do
    writeLog aInfoChan [SyncTag] Info $  "Process of syncronization with the " ++ show x
    aMyTail  <- takeMyTail aDBActorChan
    let Response aNodeId (aNumber, _) = x
    if aMyTail < aNumber then do
        writeLog aInfoChan [SyncTag] Info $  "My tail is small."
        lastCommonNumber <- findBeforeFork aMyTail aNodeId outSyncChan aDBActorChan aManagerChan aInfoChan
        aDiffBlock <- takeRecords aDBActorChan (GetKeyBlockSproutData (lastCommonNumber+1) (lastCommonNumber+1))
        aOk <- loadBlocks outSyncChan aDBActorChan aManagerChan aDiffBlock (lastCommonNumber+1) aNumber aNodeId aInfoChan
        unless aOk $ syncNeighbors outSyncChan aDBActorChan aManagerChan xs aInfoChan
    else return ()
syncNeighbors _ _ _ _ _ = return ()

syncServer :: (InChan SyncEvent, OutChan SyncEvent) -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO b
syncServer (_, outSyncChan) aDBActorChan aManagerChan aInfoChan = do
    writeLog aInfoChan [SyncTag, InitTag] Info "Init syncServer"
    forever $
      readChan outSyncChan >>= \case
        RestartSync -> syncProcess outSyncChan aDBActorChan aManagerChan aInfoChan

        SyncMsg aNodeId aMsg -> do
            let aSend amsg = sendMsgToNode aManagerChan amsg aNodeId
            case aMsg of
                RequestTail                                 -> do
                    takeRecords aDBActorChan MyTail >>= \case
                        Just aTail  -> aSend $ ResponseTail aTail
                        Nothing     -> aSend $ StatusSyncMessage "Empty tail" "#001"

                PeekKeyBlokRequest aFrom aTo                -> do
                    takeRecords aDBActorChan (GetKeyBlockSproutData aFrom aTo) >>=
                        aSend . PeekKeyBlokResponse

                MicroblockRequest aHash         -> do
                    takeRecords aDBActorChan (GetRestSproutData aHash) >>= \case
                      Just mblock -> aSend $ MicroblockResponse mblock
                      Nothing     -> aSend $ StatusSyncMessage ("No block with this hash " ++ show aHash ++ " in DB")  "#003"

                PeekHashKblokRequest aFrom aTo -> do
                    takeRecords aDBActorChan (PeekNKeyBlocks aFrom aTo) >>=
                        aSend . PeekHashKblokResponse


                _ -> aSend $ StatusSyncMessage ("Send command are not allowed for server")  "#005"


sendMsgToNode :: ToJSON a => InChan MsgToCentralActor -> a -> NodeId -> IO ()
sendMsgToNode aChan aMsg aId = writeInChan aChan $ SendMsgToNode (toJSON aMsg) (IdTo aId)


data Response a = Response NodeId a deriving Show

takeResponseTail :: OutChan SyncEvent -> IO (Response (Number, Maybe HashOfKeyBlock))
takeResponseTail aChan = readChan aChan >>= \case
    SyncMsg aId aMsg  -> case aMsg of
        ResponseTail (aNum, aHash) -> return $ Response aId (aNum,Just aHash)
        StatusSyncMessage _ "#001" -> return $ Response aId (0, Nothing)
        _                          -> takeResponseTail aChan
    _                 -> takeResponseTail aChan

takePeekKeyBlokResponse     :: OutChan SyncEvent -> IO (Response [(Number, HashOfKeyBlock, MacroblockBD)])
takePeekKeyBlokResponse aChan = readChan aChan >>= \case
    SyncMsg aId aMsg  -> case aMsg of
        PeekKeyBlokResponse aList  -> return $ Response aId aList
        StatusSyncMessage _ "#002" -> return $ Response aId []
        _                          -> takePeekKeyBlokResponse aChan
    _                 -> takePeekKeyBlokResponse aChan

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



takeRecords :: InChan a -> (MVar p -> a) -> IO p
takeRecords aChan aTaker  = do
    aVar <- newEmptyMVar
    writeInChan aChan $ aTaker aVar
    takeMVar aVar

connectToNN
    ::  InChan FileActorRequest
    ->  NodeId
    ->  InChan PendingAction
    ->  InChan InfoMsg
    ->  InChan MsgToCentralActor
    ->  Connect
    ->  IO ()
connectToNN aFileServerChan aMyNodeId inChanPending aInfoChan ch aConn@(Connect aIp aPort)= do
    aOk <- try $ runClient (showHostAddress aIp) (fromEnum aPort) "/" $ \aConnect -> do
        WS.sendTextData aConnect . A.encode $ ActionConnect NN (Just aMyNodeId)
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right (ActionConnect aNodeType (Just aNodeId)) -> do
                (aInpChan, aOutChan) <- newChan 64
                sendActionToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing
                void $ race
                    (msgSender ch aMyNodeId aConnect aOutChan)
                    (msgReceiver ch aInfoChan aFileServerChan NN (IdFrom aNodeId) aConnect inChanPending)
            _ -> return ()

    case aOk of
        Left (_ :: SomeException) ->
            void $ tryWriteChan aFileServerChan $ DeleteFromFile aConn
        _ -> return ()



readNodeConfig :: IO NodeConfig
readNodeConfig =
    try (L.readFile "configs/nodeInfo.json") >>= \case
        Right nodeConfigMsg         -> case decode nodeConfigMsg of
            Just nodeConfigData     -> return nodeConfigData
            Nothing                 -> putStrLn "Config file can not be readed. New one will be created" >> config
        Left (_ :: SomeException)   -> putStrLn "ConfigFile will be created." >> config
  where
    config = do
        makeFileConfig
        readNodeConfig

readBootNodeList :: String -> IO [Connect]
readBootNodeList conf = do
    bnList  <- try (getEnv "bootNodeList") >>= \case
            Right item              -> return item
            Left (_::SomeException) -> return conf
    toNormForm $ read bnList
     where
       toNormForm aList = return $ (\(b,c) -> Connect (tupleToHostAddress b) c)
          <$> aList


mergeMBlocks :: [Microblock] -> [Microblock] -> [Microblock] -- new old result
mergeMBlocks [] old = old
mergeMBlocks (x:xs) olds = if containMBlock x olds
    then mergeMBlocks xs olds
    else mergeMBlocks xs (x : olds)


containMBlock :: Microblock -> [Microblock] -> Bool
containMBlock el elements = or $ (==) <$> [el] <*> elements
-------------------------------------------------------

getConfigParameters
    :: Show a1
    => a1
    ->  BuildConfig
    ->  InChan MsgToCentralActor
    ->  IO (SimpleNodeBuildConfig, PortNumber, String, PortNumber, String, PortNumber, String)
getConfigParameters aMyNodeId conf _ = do
  snbc    <- try (pure $ fromJust $ simpleNodeBuildConfig conf) >>= \case
          Right item              -> return item
          Left (_::SomeException) -> error "Please, specify simpleNodeBuildConfig"

  poa_p   <- try (getEnv "poaPort") >>= \case
          Right item              -> return $ read item
          Left (_::SomeException) -> return $ poaPort conf

  stat_h  <- try (getEnv "statsdHost") >>= \case
          Right item              -> return item
          Left (_::SomeException) -> return $ host $ statsdBuildConfig conf

  stat_p  <- try (getEnv "statsdPort") >>= \case
          Right item              -> return $ read item
          Left (_::SomeException) -> return $ port $ statsdBuildConfig conf

  logs_h  <- try (getEnv "logHost") >>= \case
          Right item              -> return item
          Left (_::SomeException) -> return $ host $ logsBuildConfig conf

  logs_p  <- try (getEnv "logPort") >>= \case
          Right item              -> return $ read item
          Left (_::SomeException) -> return $ port $ logsBuildConfig conf

  log_id  <- try (getEnv "log_id") >>= \case
          Right item              -> return item
          Left (_::SomeException) -> return $ show aMyNodeId

  return (snbc, poa_p, stat_h, stat_p, logs_h, logs_p, log_id)
