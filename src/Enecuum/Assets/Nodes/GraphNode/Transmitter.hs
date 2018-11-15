module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Assets.Nodes.Address  as A 
import           Enecuum.Config
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.GraphNode.Database
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
    let myNodePorts   = _gnNodePorts cfg
    let bnNodeAddress = Enecuum.Assets.Nodes.GraphNode.Config._bnAddress cfg

    -- TODO: read from config
    let myHash      = D.toHashGeneric myNodePorts
    
    routingData <- L.scenario $ makeRoutingRuntimeData myNodePorts myHash bnNodeAddress

    periodic (1000 * 1000) $ do
        connects <- fromChordRouteMap <$> L.readVarIO (routingData ^. connectMap)
        forM_ connects $ \(_, nodeAddress) -> do
            let rpcAddress = A.getRpcAddress nodeAddress
            eNodeType <- L.makeRpcRequest rpcAddress M.GetNodeType
            whenRight eNodeType $ \nodeType ->
                when (nodeType == M.TypeGraphNode) $ graphSynchro nodeData rpcAddress

    udpServerOk <- L.serving D.Udp (myNodePorts ^. A.nodeUdpPort) $ do
        udpRoutingHandlers routingData
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ udpBroadcastRecivedMessage routingData (acceptMBlock' nodeData)
        -- PoW interaction
        L.handler $ udpBroadcastRecivedMessage routingData (acceptKBlock' nodeData)

    tcpServerOk <- L.serving D.Tcp (myNodePorts ^. A.nodeTcpPort) $
        -- network
        L.handler   methodPing

    rpcServerOk <- L.serving D.Rpc (myNodePorts ^. A.nodeRpcPort) $ do
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

    if all isJust [rpcServerOk, udpServerOk, tcpServerOk] then do
        routingWorker routingData
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
    else do
        unless (isJust rpcServerOk) $
            L.logError $ portError (myNodePorts ^. A.nodeRpcPort) "rpc" 
        unless (isJust udpServerOk) $
            L.logError $ portError (myNodePorts ^. A.nodeUdpPort) "udp"
        unless (isJust tcpServerOk) $
            L.logError $ portError (myNodePorts ^. A.nodeTcpPort) "tcp"

