{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Samples.Assets.Nodes.TstNodes.GraphNode.Config where

import qualified Data.Aeson                               as A
import           Enecuum.Samples.Assets.Nodes.Address
import           Enecuum.Samples.Assets.Nodes.GraphService.Config
import           Enecuum.Config
import           Enecuum.Domain                           (NodePorts (..))
import qualified Enecuum.Domain                           as D
import           Enecuum.Prelude

data TstGraphNode = TstGraphNode
    deriving (Show, Generic)

data instance NodeConfig TstGraphNode = TstGraphNodeConfig
    { _graphServiceConfig :: GraphServiceConfig
    , _nodePorts          :: NodePorts
    }
    deriving (Show, Generic)

instance ToJSON   TstGraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstGraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstGraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions


tstGraphNodeTransmitterConfig :: D.NodeConfig TstGraphNode
tstGraphNodeTransmitterConfig = TstGraphNodeConfig
  { _graphServiceConfig = GraphServiceConfig
      { _graphWindowConfig = noGraphShrinking
      , _dbConfig = noDBConfig
      , _rpcSynco = Nothing
      }
  , _nodePorts = tstGraphNodeTransmitterPorts
  }

tstGraphNodeReceiverConfig :: D.NodeConfig TstGraphNode
tstGraphNodeReceiverConfig = TstGraphNodeConfig
  { _graphServiceConfig = GraphServiceConfig
      { _graphWindowConfig = noGraphShrinking
      , _dbConfig = noDBConfig
      , _rpcSynco = Just $ getRpcAddress tstGraphNodeTransmitterAddress
      }
  , _nodePorts = tstGraphNodeReceiverPorts
  }
