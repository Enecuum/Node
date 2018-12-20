{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}

module Enecuum.Samples.Assets.Nodes.TstNodes.GraphNode.Node where

import qualified Data.Aeson                                         as A
import qualified Data.Map                                           as Map
import qualified Enecuum.Samples.Assets.Nodes.Address                       as A
import qualified Enecuum.Samples.Assets.Nodes.CLens                         as CLens
import           Enecuum.Samples.Assets.Nodes.GraphService.Config
import           Enecuum.Samples.Assets.Nodes.GraphService.DB.Dump          (dumpToDB)
import           Enecuum.Samples.Assets.Nodes.GraphService.DB.Restore       (restoreFromDB)
import           Enecuum.Samples.Assets.Nodes.GraphService.GraphServiceData
import           Enecuum.Samples.Assets.Nodes.GraphService.Initialization
import           Enecuum.Samples.Assets.Nodes.GraphService.Logic
import           Enecuum.Samples.Assets.Nodes.Methods
import           Enecuum.Samples.Assets.Nodes.TstNodes.GraphNode.Config
import           Enecuum.Config
import qualified Enecuum.Domain                                     as D
import           Enecuum.Framework.Language.Extra                   (HasStatus)
import qualified Enecuum.Framework.Lens                             as Lens
import qualified Enecuum.Language                                   as L
import           Enecuum.Prelude

instance Node TstGraphNode where
    data NodeScenario TstGraphNode = GN
        deriving (Show, Generic)
    getNodeScript GN = tstGraphNode
    getNodeTag _ = TstGraphNode

instance ToJSON   (NodeScenario TstGraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

data TstGraphNodeData = TstGraphNodeData
    { _graphServiceData :: GraphServiceData
    , _status           :: D.StateVar D.NodeStatus
    }

-- | Start of graph node
tstGraphNode :: NodeConfig TstGraphNode -> L.NodeDefinitionL ()
tstGraphNode cfg@(TstGraphNodeConfig graphServiceCfg _) = do
    L.setNodeTag "Test Graph Node"

    status <- L.newVarIO D.NodeActing
    eGraphServiceData <- graphServiceInitialization graphServiceCfg
    case eGraphServiceData of
        Left err               -> L.logError err
        Right graphServiceData -> do
            let nodeData = TstGraphNodeData graphServiceData status
            tstGraphNode' cfg nodeData

tstGraphNode' :: NodeConfig TstGraphNode -> TstGraphNodeData -> L.NodeDefinitionL ()
tstGraphNode' cfg nodeData@(TstGraphNodeData graphServiceData _) = do
    let graphServiceConfig = _graphServiceConfig cfg

    case _rpcSynco graphServiceConfig  of
        Nothing              -> pure ()
        Just rpcSyncoAddress -> L.process $ forever $ graphSynchro graphServiceData rpcSyncoAddress

    L.serving D.Udp (_nodePorts cfg ^. Lens.nodeUdpPort) $ do
        -- network
        L.handler   methodPing
        -- PoA interaction
        L.handler $ acceptMBlock graphServiceData
        -- PoW interaction
        L.handler $ acceptKBlock graphServiceData

    L.serving D.Tcp (_nodePorts cfg ^. Lens.nodeTcpPort) $
        -- network
        L.handler   methodPing

    L.serving D.Rpc (_nodePorts cfg ^. Lens.nodeRpcPort) $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

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

        -- PoW interaction
        L.method  $ getKBlockPending graphServiceData

        -- PoA interaction
        L.method  $ getTransactionPending graphServiceData
        L.method  $ getLastKBlock graphServiceData

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

    when (graphServiceConfig ^. CLens.graphWindowConfig . CLens.shrinkingEnabled) $ do
        let windowSize     = graphServiceConfig ^. CLens.graphWindowConfig . CLens.windowSize
        let shrinkingDelay = graphServiceConfig ^. CLens.graphWindowConfig . CLens.shrinkingDelay
        L.process $ forever $ do
            L.delay shrinkingDelay
            shrinkGraphWindow graphServiceData windowSize

    L.awaitNodeFinished nodeData

makeFieldsNoPrefix ''TstGraphNodeData
