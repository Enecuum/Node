{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}

module Enecuum.Assets.Nodes.GN where

import qualified Data.Aeson                                         as A
import qualified Enecuum.Assets.Nodes.Address                       as A
import           Enecuum.Assets.Nodes.GraphService.Config
import           Enecuum.Assets.Nodes.GraphService.DB.Dump
import           Enecuum.Assets.Nodes.GraphService.DB.Restore
import           Enecuum.Assets.Nodes.GraphService.GraphServiceData
import           Enecuum.Assets.Nodes.GraphService.Initialization
import           Enecuum.Assets.Nodes.GraphService.Logic
import qualified Enecuum.Assets.Nodes.Messages                      as M
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Assets.Nodes.Routing                       as R
import           Enecuum.Config
import qualified Enecuum.Domain                                     as D
import           Enecuum.Framework.Language.Extra                   (HasStatus)
import qualified Enecuum.Framework.Lens                             as Lens
import qualified Enecuum.Language                                   as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

data GraphNode = GraphNode
    deriving (Show, Generic)

data instance NodeConfig GraphNode = GraphNodeConfig
    { _graphServiceConfig :: GraphServiceConfig
    , _nodePorts          :: D.NodePorts
    , _bnAddress          :: D.NodeAddress
    }
    deriving (Show, Generic)

instance Node GraphNode where
    data NodeScenario GraphNode = GN
        deriving (Show, Generic)
    getNodeScript GN = graphNode
    getNodeTag _ = GraphNode

instance ToJSON   GraphNode                where toJSON     = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphNode                where parseJSON  = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig GraphNode)   where toJSON     = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GraphNode)   where parseJSON  = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario GraphNode) where toJSON     = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario GraphNode) where parseJSON  = A.genericParseJSON nodeConfigJsonOptions

data GraphNodeData = GraphNodeData
    { _graphServiceData :: GraphServiceData
    , _status           :: D.StateVar D.NodeStatus
    }

-- | Start of graph node
graphNode :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNode cfg@(GraphNodeConfig graphServiceCfg _ _)  = do
    L.nodeTag "Routing Graph Node"

    status <- L.newVarIO D.NodeActing
    eGraphServiceData <- graphServiceInitialization graphServiceCfg
    case eGraphServiceData of
        Left err               -> L.logError err
        Right graphServiceData -> do
            let nodeData = GraphNodeData graphServiceData status
            graphNode' cfg nodeData

graphNode' :: NodeConfig GraphNode -> GraphNodeData -> L.NodeDefinitionL ()
graphNode' cfg nodeData@(GraphNodeData graphServiceData _) = do
    let myNodePorts = _nodePorts cfg
    let bnAddress   = _bnAddress cfg
    -- TODO: read from config
    let myHash      = D.toHashGeneric myNodePorts

    routingData <- R.runRouting myNodePorts myHash bnAddress

    L.periodic (1000 * 1000) $ do
        connects <- fromChordRouteMap <$> L.readVarIO (routingData ^. R.connectMap)
        forM_ connects $ \(_, nodeAddress) -> do
            let rpcAddress = A.getRpcAddress nodeAddress
            eNodeType <- L.makeRpcRequest rpcAddress M.GetNodeType
            whenRight eNodeType $ \nodeType ->
                when (nodeType == M.TypeGraphNode) $ graphSynchro graphServiceData rpcAddress

    udpServerOk <- L.serving D.Udp (myNodePorts ^. Lens.nodeUdpPort) $ do
        R.udpRoutingHandlers routingData
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ R.udpBroadcastReceivedMessage routingData (acceptMBlock' graphServiceData)
        -- PoW interaction
        L.handler $ R.udpBroadcastReceivedMessage routingData (acceptKBlock' graphServiceData)

    tcpServerOk <- L.serving D.Tcp (myNodePorts ^. Lens.nodeTcpPort) $
        -- network
        L.handler   methodPing

    rpcServerOk <- L.serving D.Rpc (myNodePorts ^. Lens.nodeRpcPort) $ do
        R.rpcRoutingHandlers routingData
        -- network
        L.method  $ handleStopNode nodeData
        L.method  $ \M.GetNodeType -> pure M.TypeGraphNode

        -- client interaction
        L.methodE $ getBalance graphServiceData
        L.methodE $ acceptTransaction graphServiceData

        -- db
        L.methodE $ handleDumpToDB graphServiceData
        L.methodE $ handleRestoreFromDB graphServiceData

        -- graph node interaction
        L.method  $ acceptGetChainLengthRequest graphServiceData
        L.methodE $ acceptChainFromTo graphServiceData
        L.methodE $ getMBlockForKBlocks graphServiceData
        L.method  $ synchronize graphServiceData

        -- PoW interaction
        L.method  $ getKBlockPending graphServiceData

        -- PoA interaction
        L.method  $ getTransactionPending graphServiceData
        L.method  $ getLastKBlock graphServiceData

    if all isJust [rpcServerOk, udpServerOk, tcpServerOk] then do
        L.std $ L.stdHandler $ L.stopNodeHandler nodeData

        L.process $ forever $ do
            L.awaitSignal $ graphServiceData ^. dumpToDBSignal
            dumpToDB graphServiceData

        L.process $ forever $ do
            L.awaitSignal $ graphServiceData ^. restoreFromDBSignal
            restoreFromDB graphServiceData

        L.process $ forever $ do
            L.awaitSignal $ graphServiceData ^. checkPendingSignal
            blockFound <- processKBlockPending' graphServiceData
            when blockFound $ L.writeVarIO (graphServiceData ^. checkPendingSignal) True

        L.awaitNodeFinished nodeData

    else do
        unless (isJust rpcServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeRpcPort) "rpc"
        unless (isJust udpServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeUdpPort) "udp"
        unless (isJust tcpServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeTcpPort) "tcp"

defaultGraphNodeConfig :: NodeConfig GraphNode
defaultGraphNodeConfig = GraphNodeConfig
    { _graphServiceConfig = GraphServiceConfig
        { _dbConfig = noDBConfig
        , _rpcSynco = Nothing
        }
    , _nodePorts  = A.defaultGnNodePorts
    , _bnAddress  = A.defaultBnNodeAddress
    }

makeFieldsNoPrefix ''GraphNodeData
