{-# LANGUAGE DuplicateRecordFields  #-}
module Enecuum.Assets.Nodes.MultiNode.Config where

import qualified Data.Aeson                           as J
import           Enecuum.Config
import           Enecuum.Prelude
import qualified Enecuum.Domain                       as D

data MultiNode = MultiNode
    deriving (Show, Generic)

data instance NodeConfig MultiNode = MultiNodeConfig
        { _nnPorts  :: D.Range D.PortNumber
        , _poaPorts :: D.Range D.PortNumber
        , _powPorts :: D.Range D.PortNumber
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig MultiNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig MultiNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   MultiNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON MultiNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

instance Node MultiNode where
    data NodeScenario MultiNode = MultiNodeS
        deriving (Show, Generic)
    getNodeScript _ = error "it not impemented"

instance ToJSON   (NodeScenario MultiNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario MultiNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

defaultMultiNodeConfig = MultiNodeConfig (D.newRange 5050 5070) D.newEmptyRange D.newEmptyRange


