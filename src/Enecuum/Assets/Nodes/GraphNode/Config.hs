module Enecuum.Assets.Nodes.GraphNode.Config where

import           Enecuum.Prelude

import qualified Data.Aeson as A

import           Enecuum.Config
import qualified Enecuum.Domain as D

data GraphNode = GraphNode
    deriving (Show, Generic)

data instance NodeConfig GraphNode = GraphNodeConfig
    { _dbModel     :: FilePath
    , _dbOptions   :: D.DBOptions
    , _useDatabase :: Bool
    }
    deriving (Show, Generic)

instance ToJSON   GraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig GraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

