module Enecuum.Assets.Nodes.OldNodes.GraphNode.Config where

import qualified Data.Aeson                            as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Config
import qualified Enecuum.Domain                        as D
import           Enecuum.Prelude

data OldGraphNode = OldGraphNode
    deriving (Show, Generic)

data instance NodeConfig OldGraphNode = OldGraphNodeConfig
    { _dbConfig     :: DBConfig
    , _gnNodePorts  :: NodePorts
    , _bnAddress    :: NodeAddress
    , _rpcSynco     :: Maybe D.Address
    }
    deriving (Show, Generic)

instance ToJSON   OldGraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON OldGraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig OldGraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig OldGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
