{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.Initialize where

import qualified Data.Map                           as M
import qualified Enecuum.Assets.Nodes.Address       as A
import           Enecuum.Assets.Nodes.ConfigParsing (parseConfig)
import qualified Enecuum.Assets.Scenarios           as Prd
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
import           System.Random

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


-- TODO: FIXME: nodes list is duplicating here and in ConfigParsing.
initialize :: LByteString -> IO ()
initialize configSrc = do
    -- Try to parse config of unknown type (parseConfig failure invoke error)
    parseConfig configSrc
    -- Figure out type of node and run appropriate script

    -- Don't forget to update the list in ConfigParsing!
    let runners =
            [ runNode' $ Cfg.dispatchScenario @Prd.GraphNode  configSrc
            , runNode' $ Cfg.dispatchScenario @Prd.GenPoANode configSrc
            , runNode' $ Cfg.dispatchScenario @Prd.GenPoWNode configSrc
            , runNode' $ Cfg.dispatchScenario @Prd.BootNode   configSrc

            , runNode' $ Cfg.dispatchScenario @Prd.ClientNode configSrc

            , runNode' $ Cfg.dispatchScenario @Tst.TestClient configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TestServer configSrc

            , runNode' $ Cfg.dispatchScenario @Tst.TstNetworkNode configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstGraphNode   configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstGenPoWNode  configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstGenPoANode  configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstRealPoWNode configSrc
            ]
    sequence_ runners

runMultiNode :: LByteString -> IO ()
runMultiNode configSrc = case Cfg.dispatchScenario @Prd.MultiNode configSrc of
    Just (cfg, _) -> do
        startNodes "pow" startGenPoWNode
            (Prd._routingGenPoWPorts $ Cfg.nodeConfig cfg)
            (Prd._routingGenPoWConfig $ Cfg.nodeConfig cfg)
        
        startNodes "poa" startGenPoaNode
            (Prd._routingGenPoAPorts $ Cfg.nodeConfig cfg)
            (Prd._routingGenPoAConfig $ Cfg.nodeConfig cfg)
        
        startNodes "gn" startGrapNode
            (Prd._routingGraphNodePorts $ Cfg.nodeConfig cfg)
            (Prd._routingGraphNodeConfig $ Cfg.nodeConfig cfg)
        forever $ threadDelay 50000000

    Nothing -> putTextLn "Parse error of multi node config."

startNodes tag act range cfg = do
    putTextLn $ "Start " <> tag <> " in range from " <> show (D.bottomBound range) <> " to " <> show (D.topBound range)
    forM_ (D.rangeToList range) $ \port -> do
        threadDelay 3000
        rand :: Int <- randomIO
        void $ forkIO $ void $ act cfg (D.toHashGeneric rand) port

startGenPoWNode :: Cfg.NodeConfig Prd.GenPoWNode -> D.NodeId -> D.PortNumber -> IO ()
startGenPoWNode cfg rand port = do
    let nodeCfg = cfg
            { Prd._powNodePorts = A.makeNodePorts1000 port
            , Prd._powNodeId    = rand
            }
    runNode D.nullLoger (Prd.powNode' nodeCfg)

startGenPoaNode :: Cfg.NodeConfig Prd.GenPoANode -> D.NodeId -> D.PortNumber -> IO ()
startGenPoaNode cfg rand port = do
    let nodeCfg = cfg
            { Prd._poaNodePorts = A.makeNodePorts1000 port
            , Prd._poaNodeId = rand
            }
    runNode D.nullLoger (Prd.poaNode Prd.Good nodeCfg)

startGrapNode :: Cfg.NodeConfig Prd.GraphNode -> D.NodeId -> D.PortNumber ->  IO ()
startGrapNode cfg rand port = do
    let nodeCfg = cfg
            { Prd._nodePorts   = A.makeNodePorts1000 port
            , Prd._graphNodeId = rand
            }
    runNode D.nullLoger (Prd.graphNode nodeCfg)
