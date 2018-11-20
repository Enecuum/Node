{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.Initialize where

import qualified Data.Map                        as M
import           Enecuum.Prelude

import qualified Enecuum.Assets.Scenarios        as S
import           Enecuum.Assets.System.Directory (clientStory)
import qualified Enecuum.Config                  as Cfg
import qualified Enecuum.Core.Lens               as Lens
import           Enecuum.Interpreters            (clearNodeRuntime, runFileSystemL, runNodeDefinitionL)
import qualified Enecuum.Language                as L
import qualified Enecuum.Runtime                 as R
import qualified Enecuum.Domain                  as D
import           Enecuum.Runtime                 (clearCoreRuntime, clearLoggerRuntime,
                                                  createCoreRuntime, createLoggerRuntime, createNodeRuntime)

createLoggerRuntime' :: D.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' loggerConfig' = do
    let logFile = loggerConfig' ^. Lens.logFilePath
    putStrLn @Text $ "Log file: " +| logFile |+ "."
    putStrLn @Text "Creating logger runtime..."
    createLoggerRuntime loggerConfig'

clearLoggerRuntime' :: R.LoggerRuntime -> IO ()
clearLoggerRuntime' loggerRt = do
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt

createCoreRuntime' :: R.LoggerRuntime -> IO R.CoreRuntime
createCoreRuntime' loggerRt = do
    putStrLn @Text "Creating core runtime..."
    createCoreRuntime loggerRt

clearCoreRuntime' :: R.CoreRuntime -> IO ()
clearCoreRuntime' coreRt = do
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt

createNodeRuntime' :: R.CoreRuntime -> IO R.NodeRuntime
createNodeRuntime' coreRt = do
    story <- runFileSystemL clientStory
    putStrLn @Text "Creating node runtime..."
    createNodeRuntime coreRt (M.singleton "Client" story)

clearNodeRuntime' :: R.NodeRuntime -> IO ()
clearNodeRuntime' nodeRt = do
    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt

runNode :: D.LoggerConfig -> L.NodeDefinitionL () -> IO ()
runNode loggerConfig node =
    bracket (createLoggerRuntime' loggerConfig) clearLoggerRuntime' $ \loggerRt ->
    bracket (createCoreRuntime'   loggerRt)     clearCoreRuntime'   $ \coreRt   ->
    bracket (createNodeRuntime'   coreRt)       clearNodeRuntime'   $ \nodeRt   ->
    runNodeDefinitionL nodeRt node

runNode'
    :: Show node
    => Show (Cfg.NodeConfig node)
    => Show (Cfg.NodeScenario node)
    => Maybe (Cfg.Config node, L.NodeDefinitionL ())
    -> IO (Maybe ())
runNode' Nothing = pure Nothing
runNode' (Just (cfg, node)) = do
    putStrLn @Text $
        "Starting node..." <>
        "\n    Node:     " +|| Cfg.node cfg         ||+
        "\n    Scenario: " +|| Cfg.nodeScenario cfg ||+ ""
    let loggerConfig' = Cfg.loggerConfig cfg
    runNode loggerConfig' node
    pure $ Just ()

initialize :: LByteString -> IO ()
initialize configSrc = do
    let runners =
            [ runNode' $ dispatchScenario @S.GraphNode  configSrc
            , runNode' $ dispatchScenario @S.PoANode    configSrc
            , runNode' $ dispatchScenario @S.PoWNode    configSrc
            , runNode' $ dispatchScenario @S.ClientNode configSrc
            , runNode' $ dispatchScenario @S.NN         configSrc
            , runNode' $ dispatchScenario @S.BN         configSrc
            , runNode' $ dispatchScenario @S.TestClient configSrc
            , runNode' $ dispatchScenario @S.TestServer configSrc
            ]
            
    results <- sequence runners
    case catMaybes results of
        [] -> putStrLn @Text "Invalid config passed: node not found."
        _  -> pure ()

getNodeScript' :: Cfg.Node node => Cfg.Config node -> L.NodeDefinitionL ()
getNodeScript' cfg = Cfg.getNodeScript (Cfg.nodeScenario cfg) (Cfg.nodeConfig cfg)

dispatchScenario
    :: FromJSON node
    => FromJSON (Cfg.NodeScenario node)
    => FromJSON (Cfg.NodeConfig node)
    => Cfg.Node node
    => LByteString
    -> Maybe (Cfg.Config node, L.NodeDefinitionL ())
dispatchScenario configSrc = case Cfg.tryParseConfig configSrc of
    Just cfg -> Just (cfg, getNodeScript' cfg)
    Nothing  -> Nothing
