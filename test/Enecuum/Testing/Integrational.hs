{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}

module Enecuum.Testing.Integrational where
    
import           Data.Aeson
import qualified Data.Map             as M
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory     as Dir
import qualified System.FilePath      as Dir
import           System.FilePath      as FP ((</>))
import qualified "rocksdb-haskell" Database.RocksDB as Rocks

import           Enecuum.Prelude
import           Enecuum.Interpreters                         (runNodeDefinitionL)
import qualified Enecuum.Language                             as L
import qualified Enecuum.Config                               as Cfg
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Runtime                              as R
import qualified Enecuum.Framework.NodeDefinition.Interpreter as R
import qualified Enecuum.Assets.Nodes.Messages                as A
import           Enecuum.Assets.Nodes.Client                  (ClientNode)

logConfig :: FilePath -> D.LoggerConfig
logConfig file = D.LoggerConfig "$prio $loggername: $msg" D.Debug file True

testConfigFilePath :: IsString a => a
testConfigFilePath = "./configs/testConfig.json"

loadLoggerConfig :: FilePath -> IO D.LoggerConfig
loadLoggerConfig configFile = do
    configSrc <- LBS.readFile configFile
    case Cfg.tryParseConfig @ClientNode configSrc of
        Nothing  -> error $ "Invalid test config file: " <> show configFile
        Just cfg -> do
            let logConf@(D.LoggerConfig _ _ logFile _) = Cfg.loggerConfig cfg
            let dir = Dir.dropFileName logFile
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

-- TODO: add runtime clearing
startNode :: Maybe D.LoggerConfig -> L.NodeDefinitionL () -> IO ()
startNode Nothing nodeDefinition = void $ forkIO $ do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime 
    runNodeDefinitionL nodeRt nodeDefinition
    R.clearNodeRuntime nodeRt
startNode (Just loggerCfg) nodeDefinition = void $ forkIO $ do
    nodeRt <- R.createLoggerRuntime loggerCfg >>= createNodeRuntime 
    runNodeDefinitionL nodeRt nodeDefinition
    R.clearNodeRuntime nodeRt

stopNode :: D.Address -> IO ()
stopNode address = do
    eResult <- makeIORpcRequest address A.Stop
    case eResult of
        Left err           -> error $ "Stopping node " +|| address ||+ " failed: " +| err |+ "."
        Right A.SuccessMsg -> pure ()

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
        go 50 = error "Node is not ready."
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
