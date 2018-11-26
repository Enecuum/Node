{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.Initialize where

import qualified Data.Map                           as M
import           Enecuum.Assets.Nodes.ConfigParsing (parseConfig)
import qualified Enecuum.Assets.Scenarios           as A
import           Enecuum.Assets.System.Directory    (clientStory)
import qualified Enecuum.Assets.TstScenarios        as Tst
import qualified Enecuum.Config                     as Cfg
import qualified Enecuum.Core.Lens                  as Lens
import qualified Enecuum.Domain                     as D
import           Enecuum.Interpreters               (clearNodeRuntime, runFileSystemL, runNodeDefinitionL)
import qualified Enecuum.Language                   as L
import           Enecuum.Prelude
import           Enecuum.Runtime                    (clearCoreRuntime, clearLoggerRuntime, createCoreRuntime,
                                                     createLoggerRuntime, createNodeRuntime)
import qualified Enecuum.Runtime                    as R

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
    -- Try to parse config of unknown type (parseConfig failure invoke error)
    parseConfig configSrc
    -- Figure out type of node and run appropriate script
    let runners =
            [ runNode' $ Cfg.dispatchScenario @A.GraphNode  configSrc
            , runNode' $ Cfg.dispatchScenario @A.PoANode    configSrc
            , runNode' $ Cfg.dispatchScenario @A.PoWNode    configSrc
            , runNode' $ Cfg.dispatchScenario @A.ClientNode configSrc
            , runNode' $ Cfg.dispatchScenario @A.BN         configSrc

            , runNode' $ Cfg.dispatchScenario @A.TestClient configSrc
            , runNode' $ Cfg.dispatchScenario @A.TestServer configSrc

            , runNode' $ Cfg.dispatchScenario @Tst.NN           configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstGraphNode configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstPoWNode   configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstPoaNode   configSrc
            ]
    void $ sequence runners
