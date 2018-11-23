{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeInType            #-}
module Enecuum.Assets.Nodes.TstNodes.GraphNode.GN where

import qualified Data.Aeson                                             as A
import qualified Data.Map                                               as Map
import qualified Enecuum.Assets.Nodes.Address                           as A
import qualified Enecuum.Assets.Nodes.GraphNode.Config                  as Prd
import qualified Enecuum.Assets.Nodes.GraphNode.Database                as Prd
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Assets.Nodes.TstNodes.GraphNode.CLens          as CLens
import           Enecuum.Assets.Nodes.TstNodes.GraphNode.Config
import           Enecuum.Assets.Nodes.TstNodes.GraphNode.Initialization
import           Enecuum.Config
import qualified Enecuum.Domain                                         as D
import qualified Enecuum.Language                                       as L
import           Enecuum.Prelude

instance Node TstGraphNode where
    data NodeScenario TstGraphNode = Transmitter | Receiver
        deriving (Show, Generic)
    getNodeScript Transmitter = graphNodeTransmitter
    getNodeScript Receiver    = graphNodeTransmitter

instance ToJSON   (NodeScenario TstGraphNode) where toJSON    = A.genericToJSON nodeConfigJsonOptions
instance FromJSON (NodeScenario TstGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

-- | Start of graph node
graphNodeTransmitter :: NodeConfig TstGraphNode -> L.NodeDefinitionL ()
graphNodeTransmitter cfg = do
    L.nodeTag "graphNodeTransmitter"
    eNodeData <- graphNodeTstInitialization cfg
    either L.logError (graphNodeTransmitter' cfg) eNodeData

graphNodeTransmitter' :: NodeConfig TstGraphNode -> TstGraphNodeData -> L.NodeDefinitionL ()
graphNodeTransmitter' cfg nodeData = do
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
        Prd.dumpToDB nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. restoreFromDBSignal
        Prd.restoreFromDB nodeData

    L.process $ forever $ do
        L.awaitSignal $ nodeData ^. checkPendingSignal
        blockFound <- processKBlockPending' nodeData
        when blockFound $ L.writeVarIO (nodeData ^. checkPendingSignal) True

    L.awaitNodeFinished nodeData
