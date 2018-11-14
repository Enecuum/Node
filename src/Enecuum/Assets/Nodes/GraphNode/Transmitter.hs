module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Config
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.GraphNode.Database

-- | Start of graph node
graphNodeTransmitter :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNodeTransmitter nodeCfg = do
    L.nodeTag "graphNodeTransmitter"
    eNodeData <- graphNodeInitialization nodeCfg
    either L.logError graphNodeTransmitter' eNodeData

graphNodeTransmitter' :: GraphNodeData -> L.NodeDefinitionL ()
graphNodeTransmitter' nodeData = do

    L.serving D.Tcp graphNodeTransmitterTcpPort $ do
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ acceptMBlock nodeData
        -- PoW interaction
        L.handler $ acceptKBlock nodeData

    -- L.serving D.Udp graphNodeTransmitterUdpPort $
    --     -- network
    --     L.handler   methodPing

    L.serving D.Rpc graphNodeTransmitterRpcPort $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

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

    L.awaitNodeFinished nodeData
