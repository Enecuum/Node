{-# LANGUAGE DuplicateRecordFields  #-}
module Enecuum.Assets.Nodes.MultiNode.Config where

import qualified Data.Aeson                           as J
import           Enecuum.Config
import           Enecuum.Prelude
import qualified Enecuum.Domain                       as D

data MultiNode = MultiNode
    deriving (Show, Generic)

data instance NodeConfig MultiNode = MultiNodeConfig
        { _nnPorts  :: Range D.PortNumber
        , _poaPorts :: Range D.PortNumber
        , _powPorts :: Range D.PortNumber
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

data Range a
    = Range
    { _minValue :: a
    , _maxValue :: a 
    }
    | EmptyRange
    deriving (Show, Generic)

newRange :: Ord a => a -> a -> Range a
newRange a b
    | a <= b    = Range a b
    | otherwise = EmptyRange

newEmptyRange :: Range a
newEmptyRange = EmptyRange

rangeToList :: Enum a => Range a -> [a] 
rangeToList (Range a b) = [a..b]
rangeToList EmptyRange  = []

instance ToJSON a   => ToJSON   (Range a) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON a => FromJSON (Range a) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
