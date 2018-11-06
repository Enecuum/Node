{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}

module Enecuum.Testing.Integrational where

import           Data.Aeson
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.Map                                     as M
import           Control.Concurrent                           (killThread)
import qualified System.Directory                             as Dir
import qualified System.FilePath                              as Dir
import           System.FilePath                              as FP ((</>))
import qualified "rocksdb-haskell" Database.RocksDB           as Rocks

import           Enecuum.Assets.Nodes.Client                  (ClientNode)
import qualified Enecuum.Assets.Nodes.Messages                as A
import qualified Enecuum.Config                               as Cfg
import qualified Enecuum.Core.Lens                            as Lens
import qualified Enecuum.Core.RLens                           as RLens
import qualified Enecuum.Framework.RLens                      as RLens
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Framework.NodeDefinition.Interpreter as R
import           Enecuum.Interpreters                         (runNodeDefinitionL)
import qualified Enecuum.Language                             as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                              as R

testLogFilePath :: IsString a => a
testLogFilePath = "/tmp/log/test.log"

consoleLoggerConfig :: D.LoggerConfig
consoleLoggerConfig = D.LoggerConfig
    { D._format       = "$prio $loggername: $msg"
    , D._level        = D.Debug
    , D._logFilePath  = testLogFilePath
    , D._logToConsole = True
    , D._logToFile    = False
    }

testConfigFilePath :: IsString a => a
testConfigFilePath = "./configs/testConfig.json"

loadLoggerConfig :: FilePath -> IO D.LoggerConfig
loadLoggerConfig configFile = do
    configSrc <- LBS.readFile configFile
    case Cfg.tryParseConfig @ClientNode configSrc of
        Nothing  -> error $ "Invalid test config file: " <> show configFile
        Just cfg -> do
            let logConf = Cfg.loggerConfig cfg
            let dir = Dir.dropFileName $ logConf ^. Lens.logFilePath
            Dir.createDirectoryIfMissing True dir
            pure logConf

createNodeRuntime :: R.LoggerRuntime -> IO R.NodeRuntime
createNodeRuntime loggerRuntime = R.createCoreRuntime loggerRuntime >>= (`R.createNodeRuntime` M.empty)

evalNode :: L.NodeDefinitionL a -> IO a
evalNode nodeDefinition = do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime
    res <- runNodeDefinitionL nodeRt nodeDefinition
    R.clearNodeRuntime nodeRt
    pure res

type NodeManager = IORef (M.Map ThreadId R.NodeRuntime)

startNode' :: IO R.LoggerRuntime -> NodeManager -> L.NodeDefinitionL () -> IO (ThreadId, R.NodeRuntime)
startNode' loggerRtAct mgr nodeDefinition = do
    loggerRt <- loggerRtAct
    nodeRt <- createNodeRuntime loggerRt
    thId <- forkIO $ runNodeDefinitionL nodeRt nodeDefinition
    modifyIORef mgr (M.insert thId nodeRt)
    pure (thId, nodeRt)

startNode :: Maybe D.LoggerConfig -> NodeManager -> L.NodeDefinitionL () -> IO (ThreadId, R.NodeRuntime)
startNode Nothing          = startNode' R.createVoidLoggerRuntime
startNode (Just loggerCfg) = startNode' (R.createLoggerRuntime loggerCfg)

stopNode :: NodeManager -> (ThreadId, R.NodeRuntime) -> IO ()
stopNode mgr (thId, nodeRt) =
    killThread thId
    `finally` R.clearNodeRuntime   nodeRt
    `finally` R.clearCoreRuntime   (nodeRt ^. RLens.coreRuntime)
    `finally` R.clearLoggerRuntime (nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime)
    `finally` modifyIORef mgr (M.delete thId)

stopNodes :: NodeManager -> IO ()
stopNodes mgr = do
    nodeRts <- readIORef mgr
    mapM_ (stopNode mgr) $ M.toList nodeRts

withNodesManager :: (NodeManager -> IO ()) -> IO ()
withNodesManager act = do
    mgr <- newIORef M.empty
    act mgr `finally` stopNodes mgr

makeIORpcRequest ::
    (FromJSON b, ToJSON a, Typeable a) => D.Address -> a -> IO (Either Text b)
makeIORpcRequest address msg = do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime
    runNodeDefinitionL nodeRt $ L.evalNodeL $ L.makeRpcRequest address msg

waitForBlocks :: D.BlockNumber -> D.Address -> IO ()
waitForBlocks number address = go 0
    where
        go :: D.BlockNumber -> IO ()
        go 50 = error "No valid results from node."
        go n = do
            threadDelay $ 1000 * 100
            A.GetChainLengthResponse count <- D.withSuccess $ makeIORpcRequest address A.GetChainLengthRequest
            when (count < number) $ go (n + 1)

waitForNode :: D.Address -> IO ()
waitForNode address = go 0
    where
        go :: D.BlockNumber -> IO ()
        go 50 = error $ "Node " +|| address ||+ "is not ready."
        go n = do
            threadDelay $ 1000 * 100
            ePong :: Either Text A.Pong <- makeIORpcRequest address A.Ping
            when (isLeft ePong) $ go (n + 1)

mkDbPath :: FilePath -> IO FilePath
mkDbPath dbName = do
    hd <- Dir.getHomeDirectory
    pure $ hd </> ".enecuum" </> dbName

rmDb :: FilePath -> IO ()
rmDb dbPath = do
    whenM (Dir.doesDirectoryExist dbPath) $ Dir.removePathForcibly dbPath
    whenM (Dir.doesDirectoryExist dbPath) $ error "Can't delete db."

mkDb :: FilePath -> IO ()
mkDb dbPath = do
    rmDb dbPath
    Dir.createDirectoryIfMissing True dbPath
    -- This creates an empty DB to get correct files in the directory.
    let opening = Rocks.open dbPath $ Rocks.defaultOptions { Rocks.createIfMissing = True
                                                           , Rocks.errorIfExists   = False
                                                           }
    bracket opening Rocks.close (const (pure ()))

withDbAbsence :: FilePath -> IO a -> IO ()
withDbAbsence dbPath act = do
    rmDb dbPath
    void act `finally` rmDb dbPath

withDbPresence :: FilePath -> IO a -> IO ()
withDbPresence dbPath act = do
    mkDb dbPath
    void act `finally` rmDb dbPath
