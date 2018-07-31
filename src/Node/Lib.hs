
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
import           Service.Transaction.Common
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
    aSyncChan@(aInputSync, _)         <- newChan 128
    aDBActorChan                                <- newChan 128

    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    (_, poa_p, _, _, _, _, _) <- getConfigParameters (config^.myNodeId) buildConf aIn

    md  <- newIORef $ makeNetworkData config infoCh aInFileRequestChan aMicroblockChan aTransactionChan aValueChan
    void . C.forkIO $ pendingActor aPendingChan aMicroblockChan outTransactionChan infoCh
    void . C.forkIO $ servePoA (config^.myNodeId) poa_p aIn infoCh aInFileRequestChan inChanPending
    void . C.forkIO $ startFileServer aOutFileRequestChan
    void . C.forkIO $ startDBActor descrDB outMicroblockChan aOutValueChan infoCh aDBActorChan aSyncChan
    void . C.forkIO $ manager aInputSync managerChan md
    void $ startDo managerChan outTransactionChan aMicroblockChan (config^.myNodeId) aInFileRequestChan

    let MyNodeId aId = config^.myNodeId

    void $ C.forkIO $ connectManager aSyncChan aDBActorChan aIn (poaPort buildConf) bnList aInFileRequestChan (NodeId aId) inChanPending infoCh
    return managerChan

sec :: Int
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
    void $ C.forkIO $ syncServer aSyncChan inDBActorChan aManagerChan aInfoChan
    forM_ aBNList $ \(Connect aBNIp aBNPort) -> do
        void . C.forkIO $ runClient (showHostAddress aBNIp) (fromEnum aBNPort) "/" $ \aConnect -> do
            WS.sendTextData aConnect . encode $ ActionAddToConnectList aPortNumber
    aConnectLoop aBNList
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

    aConnectLoop aBootNodeList  = do
        aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
        if null aActualConnects then do
            aNumberOfConnects <- takeRecords aConnectsChan NumberConnects
            when (aNumberOfConnects == 0) $ aRequestOfPotencialConnects aBootNodeList
            aConnects <- takeRecords aConnectsChan ReadRecordsFromFile
            forM_ aConnects (connectToNN aConnectsChan aMyNodeId inChanPending aInfoChan aManagerChan (fst aSyncChan))
            C.threadDelay $ 2 * sec
            aConnectLoop aBootNodeList
        else do
            C.threadDelay $ 10 * sec
            aConnectLoop aBootNodeList


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


-- загрузить блоки
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


syncServer :: (InChan SyncEvent, OutChan SyncEvent) -> InChan MsgToDB -> InChan MsgToCentralActor -> InChan InfoMsg -> IO b
syncServer (_, outSyncChan) aDBActorChan aManagerChan aInfoChan = do
    let aLog aMsg = writeLog aInfoChan [SyncTag] Info aMsg
    writeLog aInfoChan [SyncTag, InitTag] Info "Init syncServer"
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
                            aSend $ ResponseTail aTail
                        Nothing     -> do
                            aLog "Empty tail #001"
                            aSend $ StatusSyncMessage "Empty tail" "#001"

                ResponseTail (aNum, aHash) -> do
                    aLog $ "Recived response tail " ++ show aNum
                    aMyTail  <- takeMyTail aDBActorChan
                    aLog $ "My tail is: " ++ show aMyTail
                    when (aMyTail < aNum) $ do
                        aLog $ "Start of sync."
                        lastCommonNumber <- findBeforeFork aMyTail aNodeId outSyncChan aDBActorChan aManagerChan aInfoChan
                        void $ loadBlocks outSyncChan aDBActorChan aManagerChan (lastCommonNumber+1) aNum aNodeId aInfoChan

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


                _ -> aSend $ StatusSyncMessage ("Send command are not allowed for server")  "#005"


sendMsgToNode :: ToJSON a => InChan MsgToCentralActor -> a -> NodeId -> IO ()
sendMsgToNode aChan aMsg aId = writeInChan aChan $ SendMsgToNode (toJSON aMsg) (IdTo aId)


data Response a = Response NodeId a deriving Show


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
    ->  InChan SyncEvent
    ->  Connect
    ->  IO ()
connectToNN aFileServerChan aMyNodeId inChanPending aInfoChan ch aSync aConn@(Connect aIp aPort)  = do
    writeLog aInfoChan [NetLvlTag] Info $ "Try connecting to: "  ++ showHostAddress aIp
    aOk <- try $ runClient (showHostAddress aIp) (fromEnum aPort) "/" $ \aConnect -> do
        writeLog aInfoChan [NetLvlTag] Info $ "Connecting to: "  ++ showHostAddress aIp
        WS.sendTextData aConnect . A.encode $ ActionConnect NN (Just aMyNodeId)
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right (ActionConnect aNodeType (Just aNodeId))
                | aMyNodeId /= aNodeId -> do
                    (aInpChan, aOutChan) <- newChan 64
                    sendActionToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing
                    void $ C.forkIO $ do
                        C.threadDelay sec
                        writeInChan aSync RestartSync
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
