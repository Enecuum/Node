module App.Initialize where

import           Enecuum.Prelude

import           Enecuum.Config  (Config(..))
import qualified Enecuum.Core.Lens as Lens
import qualified Enecuum.Assets.Scenarios as S
import           Enecuum.Assets.System.Directory (appFileName)
import           Enecuum.Interpreters (runNodeDefinitionL)
import           Enecuum.Runtime (createNodeRuntime, createLoggerRuntime,
                                  clearNodeRuntime, clearLoggerRuntime,
                                  createCoreRuntime, clearCoreRuntime)
import           Enecuum.Assets.Nodes.Address (networkNode1Addr, networkNode2Addr)
import qualified Enecuum.Blockchain.Domain.Graph as TG
import qualified Enecuum.Framework.RLens as Lens
    -- TODO: make this more correct.
    -- TODO: use bracket idiom here

initialize :: Config -> IO ()
initialize config = do
    putStrLn @Text "Getting log file..."
    let loggerConfig' = (loggerConfig config)
    let logFile = loggerConfig' ^. Lens.logFilePath
    putStrLn @Text $ "Log file: " +| logFile |+ "."

    putStrLn @Text "Creating logger runtime..."
    loggerRt <- createLoggerRuntime $ loggerConfig'
    putStrLn @Text "Creating core runtime..."
    coreRt <- createCoreRuntime loggerRt
    putStrLn @Text "Creating node runtime..."
    nodeRt <- createNodeRuntime coreRt

    when (bootNode config) $ do
        putStrLn @Text "Starting boot node..."
        runNodeDefinitionL nodeRt $ S.bootNode config

    when (masterNode config) $ do
        putStrLn @Text "Starting master node..."
        runNodeDefinitionL nodeRt $ S.masterNode config

    when (networkNode1 config) $ do
        putStrLn @Text "Starting networkNode1 node..."
        runNodeDefinitionL nodeRt S.networkNode1

    when (networkNode2 config) $ do
        putStrLn @Text "Starting networkNode2 node..."
        graph <- TG.initGraph
        runNodeDefinitionL nodeRt $ S.networkNode2 graph

        -- TODO: this is a quick hack. Make it right.
    atomically $ readTMVar (nodeRt ^. Lens.stopNode)

    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt
