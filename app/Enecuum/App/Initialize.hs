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
import qualified Enecuum.Blockchain.Domain.Graph as TG

    -- TODO: make this more correct.
    -- TODO: use bracket idiom here

initialize :: Config -> IO ()
initialize config = do
    putStrLn @Text "Getting log file..."
    appLog <- appFileName
    putStrLn @Text $ "Log file: " +| appLog |+ "."

    let loggerConfig' = (loggerConfig config) & Lens.logFilePath .~ appLog

    putStrLn @Text "Creating logger runtime..."
    loggerRt <- createLoggerRuntime True loggerConfig'
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

    when (networkNode3 config) $ do
        putStrLn @Text "Starting networkNode3 node..."
        runNodeDefinitionL nodeRt S.networkNode3

    when (networkNode4 config) $ do
        putStrLn @Text "Starting networkNode4 node..."
        runNodeDefinitionL nodeRt $ S.networkNode4 

        -- TODO: this is a quick hack. Make it right.
    threadDelay $ 1000 * 1000 * 1000

    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt
