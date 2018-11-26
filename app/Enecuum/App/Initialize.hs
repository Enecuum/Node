{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.Initialize where

import qualified Data.Map                         as M
import           Enecuum.Assets.Nodes.ParseConfig (parseConfig)
import qualified Enecuum.Assets.Scenarios         as A
import           Enecuum.Assets.System.Directory  (clientStory)
import qualified Enecuum.Assets.TstScenarios      as Tst
import qualified Enecuum.Config                   as Cfg
import qualified Enecuum.Core.Lens                as Lens
import qualified Enecuum.Domain                   as D
import           Enecuum.Interpreters             (clearNodeRuntime, runFileSystemL, runNodeDefinitionL)
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Runtime                  (clearCoreRuntime, clearLoggerRuntime, createCoreRuntime,
                                                   createLoggerRuntime, createNodeRuntime)
import qualified Enecuum.Runtime                  as R

createLoggerRuntime' :: D.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' loggerConfig' = do
    let logFile = loggerConfig' ^. Lens.logFilePath
    putTextLn $ "Log file: " +| logFile |+ "."
    putTextLn "Creating logger runtime..."
    createLoggerRuntime loggerConfig'

clearLoggerRuntime' :: R.LoggerRuntime -> IO ()
clearLoggerRuntime' loggerRt = do
    putTextLn "Clearing logger runtime..."
    clearLoggerRuntime loggerRt

createCoreRuntime' :: R.LoggerRuntime -> IO R.CoreRuntime
createCoreRuntime' loggerRt = do
    putTextLn "Creating core runtime..."
    createCoreRuntime loggerRt

clearCoreRuntime' :: R.CoreRuntime -> IO ()
clearCoreRuntime' coreRt = do
    putTextLn "Clearing core runtime..."
    clearCoreRuntime coreRt

createNodeRuntime' :: R.CoreRuntime -> IO R.NodeRuntime
createNodeRuntime' coreRt = do
    story <- runFileSystemL clientStory
    putTextLn "Creating node runtime..."
    createNodeRuntime coreRt (M.singleton "Client" story)

clearNodeRuntime' :: R.NodeRuntime -> IO ()
clearNodeRuntime' nodeRt = do
    putTextLn "Clearing node runtime..."
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
    putTextLn $
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
            , runNode' $ Cfg.dispatchScenario @A.NN         configSrc
            , runNode' $ Cfg.dispatchScenario @A.BN         configSrc
            , runNode' $ Cfg.dispatchScenario @A.TestClient configSrc
            , runNode' $ Cfg.dispatchScenario @A.TestServer configSrc

            , runNode' $ Cfg.dispatchScenario @Tst.TstGraphNode configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstPoWNode   configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstPoaNode   configSrc
            ]
    sequence_ runners


runMultiNode :: LByteString -> IO ()
runMultiNode configSrc = case Cfg.dispatchScenario @A.MultiNode configSrc of
    Just (cfg, _) -> do
        startPoWNodes (A._powPorts $ Cfg.nodeConfig cfg) A.defaultPoWNodeConfig
        startPoANodes (A._poaPorts $ Cfg.nodeConfig cfg) A.defaultPoANodeConfig
        startNNNodes  (A._nnPorts $ Cfg.nodeConfig cfg)  A.defaultNodeConfig

    Nothing -> putTextLn "Parse error of multi node config."

startPoWNodes range cfg = do
    forM_ (A.rangeToList range) $ \nPort -> do
        let nodeCfg = cfg {A._powNodePorts = A.makeNodePorts1000 nPort}
        void $ forkIO $ void $ runNode D.defaultLoggerConfig (A.powNode' nodeCfg)

startPoANodes range cfg = do
    forM_ (A.rangeToList range) $ \nPort -> do
        let nodeCfg = cfg {A._poaNodePorts = A.makeNodePorts1000 nPort}
        void $ forkIO $ void $ runNode D.defaultLoggerConfig (A.poaNode A.Good nodeCfg)

startNNNodes range cfg = do
    forM_ (A.rangeToList range) $ \nPort -> do
        let nodeCfg = cfg {A._gnNodePorts = A.makeNodePorts1000 nPort}
        void $ forkIO $ void $ runNode D.defaultLoggerConfig (A.graphNodeTransmitter nodeCfg)
