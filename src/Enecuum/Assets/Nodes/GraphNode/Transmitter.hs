module Enecuum.Assets.Nodes.GraphNode.Transmitter where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Config
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.GraphNode.Config

-- | Start of graph node
graphNodeTransmitter :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNodeTransmitter _ = do
    L.nodeTag "graphNodeTransmitter"
    nodeData <- graphNodeInitialization

    L.serving D.Tcp graphNodeTransmitterTcpPort $ do
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ acceptMBlock nodeData
        -- PoW interaction
        L.handler $ acceptKBlock nodeData


    L.serving D.Rpc graphNodeTransmitterRpcPort $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

        -- client interaction
        L.methodE $ getBalance nodeData
        L.methodE $ acceptTransaction nodeData

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
    L.awaitNodeFinished nodeData
