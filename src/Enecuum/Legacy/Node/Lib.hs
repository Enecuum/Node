{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Enecuum.Legacy.Node.Lib where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Lens ( (^.) )
import           Data.Aeson                            as A
import           Data.IORef
import qualified Data.ByteString.Char8                 as B8
import qualified Data.ByteString.Char8                 as B8
import qualified Data.ByteString.Lazy                  as L
import           Data.Maybe
import           Data.Text                             (pack)
import           Enecuum.Legacy.Node.ConnectManager
import           Enecuum.Legacy.Node.Data.Key
import           Enecuum.Legacy.Node.DataActor
import           Enecuum.Legacy.Node.DBActor
import           Enecuum.Legacy.Node.NetLvl.Server
import           Enecuum.Legacy.Node.Node.Config.Make
import           Enecuum.Legacy.Node.Node.Types
import           Enecuum.Legacy.Pending
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Sync.SyncJson
import           Enecuum.Legacy.Service.Types
import           Prelude
import           Network.Socket                        (tupleToHostAddress)
import           Network.Socket                        (tupleToHostAddress)
import           System.Directory                      (createDirectoryIfMissing)
import           System.Environment


startNode
    :: DBPoolDescriptor
    -> BuildConfig
    -> InChan InfoMsg
    -> (  InChan SyncEvent
       -> (InChan MsgToCentralActor, OutChan MsgToCentralActor)
       -> IORef NetworkNodeData
       -> IO ()
       )
    -> (  (InChan MsgToCentralActor, OutChan MsgToCentralActor)
       -> OutChan (Transaction, MVar Bool)
       -> InChan Microblock
       -> MyNodeId
       -> InChan (DataActorRequest Connect)
       -> IO a
       )
    -> IO (InChan MsgToCentralActor, OutChan MsgToCentralActor)
startNode descrDB buildConf infoCh aManager startDo = do

    createDirectoryIfMissing False "data"


    managerChan@( aIn               , _                  ) <- newChan 128
    (             aMicroblockChan   , outMicroblockChan  ) <- newChan 128
    (             aValueChan        , aOutValueChan      ) <- newChan 128
    (             aTransactionChan  , outTransactionChan ) <- newChan 128
    (             aInFileRequestChan, aOutFileRequestChan) <- newChan 128
    aPendingChan@(inChanPending     , _                  ) <- newChan 128
    (             aInLogerChan      , aOutLogerChan      ) <- newChan 128
    aSyncChan@(   aInputSync        , _                  ) <- newChan 128
    aDBActorChan              <- newChan 128

    config                    <- readNodeConfig
    bnList                    <- readBootNodeList $ bootNodeList buildConf
    (_, poa_p, _, _, _, _, _) <- getConfigParameters (config ^. myNodeId) buildConf aIn

    md <- newIORef $ makeNetworkData config infoCh aInFileRequestChan aMicroblockChan aTransactionChan aValueChan
    void . C.forkIO $ pendingActor aPendingChan aMicroblockChan outTransactionChan infoCh
    void . C.forkIO $ netLvlServer (config ^. myNodeId) poa_p aIn infoCh aInFileRequestChan inChanPending aInLogerChan
    void . C.forkIO $ startDataActor aOutFileRequestChan
    void . C.forkIO $ startDBActor descrDB outMicroblockChan aOutValueChan infoCh aDBActorChan aSyncChan
    void . C.forkIO $ aManager aInputSync managerChan md
    void . C.forkIO $ traficLoger aOutLogerChan
    void $ startDo managerChan outTransactionChan aMicroblockChan (config ^. myNodeId) aInFileRequestChan

    let MyNodeId aId = config ^. myNodeId

    void $ C.forkIO $ connectManager aSyncChan
                                     aDBActorChan
                                     aIn
                                     (poaPort buildConf)
                                     bnList
                                     aInFileRequestChan
                                     (NodeId aId)
                                     inChanPending
                                     infoCh
                                     aInLogerChan
    pure managerChan

traficLoger :: OutChan B8.ByteString -> IO b
traficLoger aOutChan = forever $ do
    aMsg <- readChan aOutChan
    appendFile "netLog.txt" $ B8.unpack aMsg ++ "\n"


readNodeConfig :: IO NodeConfig
readNodeConfig = try (L.readFile "configs/nodeInfo.json") >>= \case
    Right nodeConfigMsg -> case decode nodeConfigMsg of
        Just nodeConfigData -> pure nodeConfigData
        Nothing             -> putStrLn ("Config file can not be readed. New one will be created" :: String) >> config
    Left (_ :: SomeException) -> putStrLn ("ConfigFile will be created." :: String) >> config
  where
    config = do
        makeFileConfig
        readNodeConfig

readBootNodeList :: String -> IO [Connect]
readBootNodeList conf = do
    bnList <- try (getEnv "bootNodeList") >>= \case
        Right item                 -> pure item
        Left  (_ :: SomeException) -> pure conf
    toNormForm $ read bnList
    where toNormForm aList = pure $ (\(b, c) -> Connect (tupleToHostAddress b) c) <$> aList


getConfigParameters
    :: Show a1
    => a1
    -> BuildConfig
    -> InChan MsgToCentralActor
    -> IO (SimpleNodeBuildConfig, PortNumber, String, PortNumber, String, PortNumber, String)
getConfigParameters aMyNodeId conf _ = do
    snbc <- try (pure $ fromJust $ simpleNodeBuildConfig conf) >>= \case
        Right item                 -> pure item
        Left  (_ :: SomeException) -> error "Please, specify simpleNodeBuildConfig"

    poa_p <- try (getEnv "poaPort") >>= \case
        Right item                 -> pure $ read item
        Left  (_ :: SomeException) -> pure $ poaPort conf

    stat_h <- try (getEnv "statsdHost") >>= \case
        Right item                 -> pure item
        Left  (_ :: SomeException) -> pure $ host $ statsdBuildConfig conf

    stat_p <- try (getEnv "statsdPort") >>= \case
        Right item                 -> pure $ read item
        Left  (_ :: SomeException) -> pure $ port $ statsdBuildConfig conf

    logs_h <- try (getEnv "logHost") >>= \case
        Right item                 -> pure item
        Left  (_ :: SomeException) -> pure $ host $ logsBuildConfig conf

    logs_p <- try (getEnv "logPort") >>= \case
        Right item                 -> pure $ read item
        Left  (_ :: SomeException) -> pure $ port $ logsBuildConfig conf

    log_id <- try (getEnv "log_id") >>= \case
        Right item                 -> pure item
        Left  (_ :: SomeException) -> pure $ show aMyNodeId

    pure (snbc, poa_p, stat_h, stat_p, logs_h, logs_p, log_id)
