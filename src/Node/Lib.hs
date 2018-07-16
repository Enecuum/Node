
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Lib where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import              Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import          PoA.Pending

import qualified    Network.WebSockets                  as WS
import           Data.Aeson                            as A
import          Service.Chan
import qualified Data.Text                              as T
import qualified Data.ByteString.Lazy                  as L
import              Service.Network.WebSockets.Client
import           Data.Maybe
import           Data.IORef
import           Lens.Micro
import           Network.Socket                        (tupleToHostAddress)
import           Node.Data.Key
import           Node.FileDB.FileServer
import           Node.Node.Config.Make
import           Node.Node.Types
import           Service.InfoMsg                       (InfoMsg)
import           Service.Network.Base
import           PoA.PoAServer
import          Control.Concurrent.Async
import           Service.Transaction.Common            (DBPoolDescriptor (..),
                                                        addMacroblockToDB,
                                                        addMicroblockToDB)
import           Service.Types                         (Microblock, Transaction)
import           System.Directory                      (createDirectoryIfMissing)
import           System.Environment
import           PoA.Types

-- code examples:
-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
-- docs:
-- https://github.com/ethereum/devp2p/blob/master/rlpx.md
-- https://github.com/ethereum/wiki/wiki/%C3%90%CE%9EVp2p-Wire-Protocol
-- https://www.stackage.org/haddock/lts-10.3/network-2.6.3.2/Network-Socket-ByteString.html

-- | Standart function to launch a node.
startNode
    :: DBPoolDescriptor
    -> BuildConfig
    -> InChan InfoMsg
    -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor) -> IORef NetworkNodeData -> IO ())
    -> ((InChan MsgToCentralActor, OutChan MsgToCentralActor) -> OutChan (Transaction, MVar Bool) -> InChan Microblock -> MyNodeId -> InChan FileActorRequest -> IO ())
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

    config  <- readNodeConfig
    bnList  <- readBootNodeList $ bootNodeList buildConf
    (snbc, poa_p, stat_h, stat_p, logs_h, logs_p, log_id) <- getConfigParameters (config^.myNodeId) buildConf aIn

    md      <- newIORef $ makeNetworkData config infoCh aInFileRequestChan aMicroblockChan aTransactionChan aValueChan
    void . C.forkIO $ pendingActor aPendingChan aMicroblockChan outTransactionChan infoCh

    void . C.forkIO $ servePoA (config^.myNodeId) poa_p aIn infoCh aInFileRequestChan aMicroblockChan inChanPending

    void . C.forkIO $ startFileServer aOutFileRequestChan
    void . C.forkIO $ microblockProc descrDB outMicroblockChan aOutValueChan infoCh
    void . C.forkIO $ manager managerChan md
    void $ startDo managerChan outTransactionChan aMicroblockChan (config^.myNodeId) aInFileRequestChan

    let MyNodeId aId = config^.myNodeId

    void $ C.forkIO $ connectManager aIn (poaPort buildConf) bnList aInFileRequestChan (NodeId aId) inChanPending infoCh
    return managerChan


--connectManager :: InChan MsgToCentralActor -> PortNumber -> [Connect] -> InChan FileActorRequest -> IO ()
connectManager aManagerChan aPortNumber aBNList aConnectsChan aMyNodeId inChanPending aInfoChan = do
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
                C.threadDelay 1000000
                aConnectLoop (aTailOfList ++ [Connect aBNIp aBNPort])
        _       -> return ()

    aConnectLoop aBNList = do
        aActualConnects <- takeRecords aManagerChan ActualConnectsToNNRequest
        if null aActualConnects then do
            aNumberOfConnects <- takeRecords aConnectsChan NumberConnects
            when (aNumberOfConnects == 0) $ aRequestOfPotencialConnects aBNList
            aConnects <- takeRecords aConnectsChan ReadRecordsFromFile
            forM_ aConnects (connectToNN aConnectsChan aMyNodeId inChanPending aInfoChan aManagerChan)
        else do
            C.threadDelay 10000000
            aConnectLoop aBNList

-- мой ид
-- чан для пендинга
-- чан для логов

takeRecords aChan aTaker  = do
    aVar <- newEmptyMVar
    writeInChan aChan $ aTaker aVar
    takeMVar aVar

{-

СН - сетевая нода.
БН - бут нода
АК - актуальный коннект
ПК - потенциальный коннект

1. Убедиться в отсутствии АК к СН, если уже есть, то закончить.
2. Убедиться в отсутствии ПК к СН.
3. Запросить список ПК к СН у БН.
4. Протестировать список ПК к СН (попытка установить соединение).
5. => 1.

-}

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


microblockProc :: DBPoolDescriptor -> OutChan Microblock -> OutChan Value -> InChan InfoMsg -> IO ()
microblockProc descriptor aMicroblockCh aValueChan aInfoCh = do
    void $ C.forkIO $ forever $ do
        aMicroblock <- readChan aMicroblockCh
        addMicroblockToDB descriptor aMicroblock aInfoCh
    forever $ do
        aValue <- readChan aValueChan
        addMacroblockToDB descriptor aValue aInfoCh


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

---



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
