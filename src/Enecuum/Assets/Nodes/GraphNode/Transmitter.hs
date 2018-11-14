module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Config
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.GraphNode.Database
import           Enecuum.Assets.Nodes.Routing.Messages
import           Enecuum.Assets.Nodes.Routing.Runtime
import           Enecuum.Research.ChordRouteMap
import qualified Enecuum.Assets.Nodes.Messages as M

-- | Start of graph node
graphNodeTransmitter :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNodeTransmitter nodeCfg = do
    L.nodeTag "graphNodeTransmitter"
    eNodeData <- graphNodeInitialization nodeCfg
    either L.logError (graphNodeTransmitter' nodeCfg) eNodeData

graphNodeTransmitter' :: NodeConfig GraphNode -> GraphNodeData -> L.NodeDefinitionL ()
graphNodeTransmitter' cfg nodeData = do
    let myNodePorts = NodePorts
            (Enecuum.Assets.Nodes.GraphNode.Config._udpPort cfg)
            (Enecuum.Assets.Nodes.GraphNode.Config._tcpPort cfg)
            (Enecuum.Assets.Nodes.GraphNode.Config._rpcPort cfg)
    let D.Address bnHost bnPort = Enecuum.Assets.Nodes.GraphNode.Config._bnAddress cfg
    let bnPorts     = makeNodePorts1000 bnPort
    let bnId        = D.toHashGeneric bnPorts

    -- TODO: read from config
    let myHash      = D.toHashGeneric myNodePorts
    
    routingData <- L.scenario $ makeRoutingRuntimeData myNodePorts myHash (NodeAddress bnHost bnPorts bnId)

    periodic (1000 * 1000) $ do
        connects <- fromChordRouteMap <$> L.readVarIO (routingData ^. connectMap)
        forM_ connects $ \(_, nodeAddress) -> do
            let rpcAddress = getRpcAddress nodeAddress
            eNodeType <- L.makeRpcRequest rpcAddress M.GetNodeType
            whenRight eNodeType $ \nodeType ->
                when (nodeType == M.TypeGraphNode) $ graphSynchro nodeData rpcAddress

    L.serving D.Udp (myNodePorts ^. udpPort) $ do
        udpRoutingHandlers routingData
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ udpBroadcastRecivedMessage routingData (acceptMBlock' nodeData)
        -- PoW interaction
        L.handler $ udpBroadcastRecivedMessage routingData (acceptKBlock' nodeData)

    L.serving D.Tcp (myNodePorts ^. tcpPort) $
        -- network
        L.handler   methodPing

    L.serving D.Rpc (myNodePorts ^. rpcPort) $ do
        rpcRoutingHandlers routingData
        -- network
        L.method  $ handleStopNode nodeData
        L.method  $ \M.GetNodeType -> pure M.TypeGraphNode

        -- client interaction
        L.methodE $ getBalance nodeData
        L.methodE $ acceptTransaction nodeData

        -- db
        L.methodE $ handleDumpToDB nodeData
        L.methodE $ handleRestoreFromDB nodeData

        -- graph node interaction
        L.method  $ getChainLength nodeData
        L.methodE $ acceptChainFromTo nodeData
        L.methodE $ getMBlockForKBlocks nodeData

        -- PoW interaction
        L.method  $ getKBlockPending nodeData

        -- PoA interaction
        L.method  $ getTransactionPending nodeData
        L.method  $ getLastKBlock nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. dumpToDBSignal
        dumpToDB nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. restoreFromDBSignal
        restoreFromDB nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. checkPendingSignal
        blockFound <- processKBlockPending' nodeData
        when blockFound $ L.writeVarIO (nodeData ^. checkPendingSignal) True
    
    routingWorker routingData
    L.awaitNodeFinished nodeData
