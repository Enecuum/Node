{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeInType            #-}
module Enecuum.Assets.Nodes.OldNodes.GraphNode.GN where

import qualified Data.Aeson                                     as A
import qualified Enecuum.Assets.Nodes.Address                   as A
import qualified Enecuum.Assets.Nodes.GraphNode.Config          as New
import           Enecuum.Assets.Nodes.GraphNode.Database
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.OldNodes.GraphNode.Config
import           Enecuum.Config
import qualified Enecuum.Domain                                 as D
import qualified Enecuum.Language                               as L
import           Enecuum.Prelude


instance Node OldGraphNode where
    data NodeScenario OldGraphNode = OldTransmitter
        deriving (Show, Generic)
    getNodeScript OldTransmitter = graphNodeTransmitter

instance ToJSON   (NodeScenario OldGraphNode) where toJSON    = A.genericToJSON nodeConfigJsonOptions
instance FromJSON (NodeScenario OldGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

transformConfig :: NodeConfig OldGraphNode -> NodeConfig New.GraphNode
transformConfig OldGraphNodeConfig{..} = New.GraphNodeConfig _dbConfig _gnNodePorts _bnAddress _rpcSynco --undefined --

transformConfig2 :: NodeConfig New.GraphNode -> NodeConfig OldGraphNode
transformConfig2 New.GraphNodeConfig{..} = OldGraphNodeConfig _dbConfig _gnNodePorts _bnAddress _rpcSynco

-- | Start of graph node
graphNodeTransmitter :: NodeConfig OldGraphNode -> L.NodeDefinitionL ()
graphNodeTransmitter cfg = do
    let nodeCfg = transformConfig cfg
    L.nodeTag "graphNodeTransmitter"
    eNodeData <- graphNodeInitialization nodeCfg
    either L.logError (graphNodeTransmitter' cfg) eNodeData

graphNodeTransmitter' :: NodeConfig OldGraphNode -> GraphNodeData -> L.NodeDefinitionL ()
graphNodeTransmitter' nodeCfg nodeData = do
    -- let cfg = transformConfig nodeCfg
    let cfg = nodeCfg
    case _rpcSynco cfg  of
        Nothing              -> pure ()
        Just rpcSyncoAddress -> L.process $ forever $ graphSynchro nodeData rpcSyncoAddress
    L.serving D.Udp (_gnNodePorts cfg ^. A.nodeUdpPort) $ do
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ acceptMBlock nodeData
        -- PoW interaction
        L.handler $ acceptKBlock nodeData

    L.serving D.Tcp (_gnNodePorts cfg ^. A.nodeTcpPort) $
        -- network
        L.handler   methodPing

    L.serving D.Rpc (_gnNodePorts cfg ^. A.nodeRpcPort) $ do
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
        L.method  $ acceptGetChainLengthRequest nodeData
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
