{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Nodes.MultiNode.Config where

import qualified Data.Aeson                                      as J
import           Enecuum.Assets.Nodes.RoutingNodes.GenPoA
import           Enecuum.Assets.Nodes.RoutingNodes.GenPoW.Config
import           Enecuum.Assets.Nodes.RoutingNodes.GraphNode
import           Enecuum.Config
import qualified Enecuum.Domain                                  as D
import           Enecuum.Prelude

data MultiNode = MultiNode
    deriving (Show, Generic)

data instance NodeConfig MultiNode = MultiNodeConfig
        { _routingGraphNodePorts   :: D.Range D.PortNumber
        , _routingGraphNodeConfig  :: NodeConfig GraphNode
        , _routingGenPoAPorts      :: D.Range D.PortNumber
        , _routingGenPoAConfig     :: NodeConfig GenPoANode
        , _routingGenPoWPorts      :: D.Range D.PortNumber
        , _routingGenPoWConfig     :: NodeConfig GenPoWNode
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig MultiNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig MultiNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   MultiNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON MultiNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

instance Node MultiNode where
    data NodeScenario MultiNode = MultiNodeS
        deriving (Show, Generic)
    getNodeScript _ = error "Multinode script not supported."

instance ToJSON   (NodeScenario MultiNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario MultiNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

routingMultiNodeConfig :: NodeConfig MultiNode
routingMultiNodeConfig = MultiNodeConfig
    (D.newRange 5050 5070)
    routingGraphNodeConfig
    D.newEmptyRange
    routingGenPoANodeConfig
    D.newEmptyRange
    routingGenPoWNodeConfig
