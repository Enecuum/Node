{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Assets.Nodes.GraphNode.Definition where

import qualified Data.Aeson                                 as A
import           Enecuum.Prelude

import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.GraphNode.Transmitter (graphNodeTransmitter)
import           Enecuum.Config

instance Node GraphNode where
    data NodeScenario GraphNode = Transmitter
        deriving (Show, Generic)
    getNodeScript Transmitter = graphNodeTransmitter

instance ToJSON   (NodeScenario GraphNode) where toJSON    = A.genericToJSON nodeConfigJsonOptions
instance FromJSON (NodeScenario GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
