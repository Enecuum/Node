{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Nodes.TstNodes.GraphNode.Config where

import qualified Data.Aeson                            as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Config
import           Enecuum.Domain                        (NodePorts (..))
import qualified Enecuum.Domain                        as D
import           Enecuum.Prelude

data TstGraphNode = TstGraphNode
    deriving (Show, Generic)

data instance NodeConfig TstGraphNode = TstGraphNodeConfig
    { _graphServiceConfig :: GraphServiceConfig
    , _gnNodePorts        :: NodePorts
    }
    deriving (Show, Generic)

instance ToJSON   TstGraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstGraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstGraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
