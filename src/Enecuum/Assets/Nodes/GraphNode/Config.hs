module Enecuum.Assets.Nodes.GraphNode.Config where

import qualified Data.Aeson as A
import           Enecuum.Prelude
import           Enecuum.Config

data GraphNode = GraphNode
    deriving (Show, Generic)

data instance NodeConfig GraphNode = GraphNodeConfig
    { database :: FilePath
    }
    deriving (Show, Generic)

instance ToJSON   GraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig GraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
