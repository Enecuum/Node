{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

-- TODO: What is this node???
module Enecuum.Assets.Nodes.TstNodes.NetworkNode (nnNode, TstNetworkNode, NodeConfig (..)) where

import qualified Data.Aeson                       as J
import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Services.Routing
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus)
import qualified Enecuum.Framework.Lens           as Lens
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude

type SenderNodeHash  = D.StringHash

data TstNetworkNodeData = TstNetworkNodeData
    { _status          :: D.StateVar D.NodeStatus
    , _routingRuntime  :: D.StateVar RoutingRuntime
    , _routingMessages :: D.StateVar [Text]
    }
makeFieldsNoPrefix ''TstNetworkNodeData

initTstNetworkNode :: RoutingRuntime -> D.StateVar D.NodeStatus -> L.NodeDefinitionL TstNetworkNodeData
initTstNetworkNode routingData nodeStatus = do
    messages            <- L.newVarIO []
    routingRuntimeData  <- L.newVarIO routingData
    pure TstNetworkNodeData
        { _status          = nodeStatus
        , _routingRuntime  = routingRuntimeData
        , _routingMessages = messages
        }


data TstNetworkNode = TstNetworkNode
    deriving (Show, Generic)

data instance NodeConfig TstNetworkNode = TstNetworkNodeConfig
    { _dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node TstNetworkNode where
    data NodeScenario TstNetworkNode = TstNetworkNodeS
        deriving (Show, Generic)
    getNodeScript TstNetworkNodeS = nnNode' Nothing
    getNodeTag _ = TstNetworkNode

instance ToJSON   TstNetworkNode                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstNetworkNode                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstNetworkNode)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstNetworkNode)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario TstNetworkNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstNetworkNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions


newtype Start = Start D.PortNumber deriving (Read)

acceptPort :: D.StateVar (Maybe D.PortNumber) -> Start -> L.NodeL Text
acceptPort portVar (Start port) = L.atomically $ do
    currentPort <- L.readVar portVar
    unless (isJust currentPort) $ L.writeVar portVar $ Just port
    pure $ if isJust currentPort
        then "Node is already running."
        else "The port is accepted, the node is started."


getRoutingMessages :: TstNetworkNodeData -> M.GetRoutingMessages -> L.NodeL [Text]
getRoutingMessages nodeData _ = L.readVarIO (nodeData ^. routingMessages)

acceptSendTo :: TstNetworkNodeData -> SendMsgTo -> D.Connection D.Udp -> L.NodeL ()
acceptSendTo nodeData message conn = do
    routingData <- L.readVarIO (nodeData^.routingRuntime)
    udpForwardIfNeeded routingData (acceptSendTo' nodeData) message conn

acceptSendTo' :: TstNetworkNodeData -> SendMsgTo -> L.NodeL ()
acceptSendTo' nodeData (SendMsgTo _ _ mes) = do
    L.atomically $ L.modifyVar (nodeData ^. routingMessages) (mes :)
    L.logInfo "I'm receiver."

nnNode :: Maybe D.PortNumber -> L.NodeDefinitionL ()
nnNode port = nnNode' port (TstNetworkNodeConfig 42)

nnNode' :: Maybe D.PortNumber -> NodeConfig TstNetworkNode -> L.NodeDefinitionL ()
nnNode' maybePort _ = do
    L.setNodeTag "TstNetworkNode node"
    L.logInfo "Starting of TstNetworkNode node"
    portVar    <- L.newVarIO maybePort
    nodeStatus <- L.newVarIO D.NodeActing
    L.std $ do
    -- network
        L.stdHandler $ acceptPort         portVar
        L.stdHandler $ L.stopNodeHandler' nodeStatus

    -- routing
    port        <- L.await portVar
    let myNodePorts = A.makeNodePorts1000 port
    let myHash      = D.toHashGeneric myNodePorts

    routingData <- runRouting myNodePorts myHash A.routingBootNodeAddress
    nodeData    <- initTstNetworkNode routingData nodeStatus

    void $ L.serving D.Udp (routingData ^. myNodeAddres . Lens.nodePorts . Lens.nodeUdpPort) $ do
        udpRoutingHandlers routingData
        L.handler $ acceptSendTo          nodeData

    void $ L.serving D.Rpc (routingData ^. myNodeAddres . Lens.nodePorts . Lens.nodeRpcPort) $ do
        rpcRoutingHandlers routingData
        L.method  $  getRoutingMessages   nodeData

    L.awaitNodeFinished nodeData
