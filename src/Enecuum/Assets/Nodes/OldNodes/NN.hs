{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.OldNodes.NN (nnNode, NN, NodeConfig (..)) where

import qualified Data.Aeson                       as J
import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus)
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.Routing

type SenderNodeHash  = D.StringHash

data NNNodeData = NNNodeData
    { _status          :: D.StateVar L.NodeStatus
    , _routingRuntime  :: D.StateVar RoutingRuntime
    , _routingMessages :: D.StateVar [Text]
    }
makeFieldsNoPrefix ''NNNodeData

initNN :: RoutingRuntime -> D.StateVar L.NodeStatus -> L.NodeDefinitionL NNNodeData
initNN routingData nodeStatus = do
    messages            <- L.newVarIO []
    routingRuntimeData  <- L.newVarIO routingData
    pure $ NNNodeData
        { _status          = nodeStatus
        , _routingRuntime  = routingRuntimeData
        , _routingMessages = messages
        }


data NN = NN
    deriving (Show, Generic)

data instance NodeConfig NN = NNConfig
    { _dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node NN where
    data NodeScenario NN = NNS
        deriving (Show, Generic)
    getNodeScript NNS = nnNode' Nothing


instance ToJSON   NN                where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON NN                where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig NN)   where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig NN)   where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario NN) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario NN) where parseJSON = J.genericParseJSON nodeConfigJsonOptions


newtype Start = Start D.PortNumber deriving (Read)

acceptPort :: D.StateVar (Maybe D.PortNumber) -> Start -> L.NodeL Text
acceptPort portVar (Start port) = L.atomically $ do
    currentPort <- L.readVar portVar
    unless (isJust currentPort) $ L.writeVar portVar $ Just port
    pure $ if isJust currentPort
        then "Node is already running."
        else "The port is accepted, the node is started."


getRoutingMessages :: NNNodeData -> M.GetRoutingMessages -> L.NodeL [Text]
getRoutingMessages nodeData _ = L.readVarIO (nodeData ^. routingMessages)

acceptSendTo :: NNNodeData -> SendMsgTo -> D.Connection D.Udp -> L.NodeL ()
acceptSendTo nodeData message conn = do
    L.close conn
    routingData <- L.readVarIO (nodeData^.routingRuntime)
    udpForwardIfNeeded routingData message $ \(SendMsgTo _ _ mes) -> do
        L.atomically $ L.modifyVar (nodeData ^. routingMessages) (mes :)
        L.logInfo "I'm receiver."

nnNode :: Maybe D.PortNumber -> L.NodeDefinitionL ()
nnNode port = nnNode' port (NNConfig 42) 

nnNode' :: Maybe D.PortNumber -> NodeConfig NN -> L.NodeDefinitionL ()
nnNode' maybePort _ = do
    L.nodeTag "NN node"
    L.logInfo "Starting of NN node"
    portVar    <- L.newVarIO maybePort
    nodeStatus <- L.newVarIO L.NodeActing
    L.std $ do
    -- network
        L.stdHandler $ acceptPort         portVar
        L.stdHandler $ L.stopNodeHandler' nodeStatus

    -- routing
    port        <- L.await portVar
    let myNodePorts = A.makeNodePorts1000 port
    let myHash      = D.toHashGeneric myNodePorts

    routingData <- runRouting myNodePorts myHash A.defaultBnNodeAddress
    nodeData    <- initNN routingData nodeStatus

    void $ L.serving D.Udp (routingData ^. myNodeAddres . A.nodePorts . A.nodeUdpPort) $ do
        udpRoutingHandlers routingData
        L.handler $ acceptSendTo          nodeData

    void $ L.serving D.Rpc (routingData ^. myNodeAddres . A.nodePorts . A.nodeRpcPort) $ do
        rpcRoutingHandlers routingData
        L.method  $  getRoutingMessages   nodeData

    L.awaitNodeFinished nodeData
