{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Enecuum.Assets.Nodes.Routing.NN (nnNode, NN, NodeConfig (..)) where

import qualified Data.Aeson                       as J
import qualified Data.Map                         as Map
import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Config
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus)
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Assets.Nodes.Routing.Runtime
import           Enecuum.Assets.Nodes.Routing.Messages

type MyNodeHash      = D.StringHash
type SenderNodeHash  = D.StringHash

data NNNodeData = NNNodeData
    { _status          :: D.StateVar L.NodeStatus
    , _routingRuntime  :: D.StateVar RoutingRuntime
    , _routingMessages :: D.StateVar [Text]
    }
makeFieldsNoPrefix ''NNNodeData

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

initNN :: RoutingRuntime -> D.StateVar L.NodeStatus -> L.NodeDefinitionL NNNodeData
initNN routingRuntime nodeStatus = L.atomically
    (NNNodeData routingRuntime nodeStatus <$> L.newVar [])


testPorts :: [D.PortNumber]
testPorts = [5001..5010]

nodesMap :: Map D.StringHash D.Address
nodesMap = Map.fromList $ map (\node -> (D.toHashGeneric node, node)) nnNodes
    where nnNodes = map (D.Address A.localhost) testPorts

acceptSendTo
    :: NNNodeData -> D.StringHash -> M.SendMsgTo -> D.Connection D.Udp -> L.NodeL ()
acceptSendTo nodeData myHash (M.SendMsgTo hash i msg) conn = do
    L.close conn
    let hashInfo = case Map.lookup hash nodesMap of
            Nothing -> show hash
            Just address -> show address
    let mes = "Received msg: \"" <>  msg <> "\" for " <> hashInfo <> " time to live " <> show i
    L.logInfo mes

    when (myHash == hash) $ do
        L.atomically $ L.modifyVar (nodeData ^. routingMessages) (mes :)
        L.logInfo "I'm receiver."

    when (i >= 0 && myHash /= hash) $ do
        rm <- L.readVarIO (nodeData ^. netNodes)
        whenJust (findNextResender hash rm) $ \(h, address) -> do
            L.logInfo $ "Resending to: " <> show h
            void $ L.notify address (M.SendMsgTo hash (i-1) msg)

connectMapRequest :: NNNodeData -> M.ConnectMapRequest -> L.NodeL [(D.StringHash, D.Address)]
connectMapRequest nodeData _ = do
    nodes <- L.readVarIO (nodeData ^. netNodes)
    pure $ fromChordRouteMap nodes

getRoutingMessages :: NNNodeData -> M.GetRoutingMessages -> L.NodeL [Text]
getRoutingMessages nodeData _ = L.readVarIO (nodeData ^. routingMessages)

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
    --nodeData    <- initNN maybePort

    -- routing
    port        <- L.await (nodeData^.portVar)
    let myNodePorts = makeNodePorts1000 port
    let myHash      = D.toHashGeneric myNodePorts
    let bnPorts     = makeNodePorts1000 5000
    let bnId        = D.toHashGeneric bnPorts
    let bnAddress   = NodeAddress "127.0.0.1" bnPorts bnId

    routingRuntime <- makeRoutingRuntimeData myNodePorts myHash bnAddress
    nodeData       <- initNN routingRuntime nodeStatus

    L.serving D.Udp (routingRuntime^.nodePorts.udpPort) $
        udpRoutingHandlers routingRuntime
    L.serving D.Rpc (routingRuntime^.nodePorts.rpcPort) $
        rpcRotingHandlers routingRuntime
        L.method  $  connectMapRequest    nodeData
        L.method  $  getRoutingMessages   nodeData

    routingWorker routingRuntime
    L.awaitNodeFinished nodeData
