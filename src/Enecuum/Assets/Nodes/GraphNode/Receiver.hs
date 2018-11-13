{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}

module Enecuum.Assets.Nodes.GraphNode.Receiver where

import           Data.HGraph.StringHashable
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Blockchain.Lens              as Lens
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.GraphNode.Config

-- | Start of graph node
graphNodeReceiver :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNodeReceiver nodeCfg = do
    L.nodeTag "graphNodeReceiver"
    eNodeData <- graphNodeInitialization nodeCfg
    either L.logError graphNodeReceiver' eNodeData

graphNodeReceiver' :: GraphNodeData -> L.NodeDefinitionL ()
graphNodeReceiver' nodeData = do
    L.process $ forever $ graphSynchro nodeData graphNodeTransmitterRpcAddress
    L.serving D.Rpc graphNodeReceiverRpcPort $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

        -- client interaction
        L.methodE $ getBalance nodeData

        -- graph node interaction
        L.method  $ getChainLength nodeData
        L.methodE $ getMBlockForKBlocks nodeData

        -- PoA interaction
        L.method  $ getLastKBlock nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.awaitNodeFinished nodeData
