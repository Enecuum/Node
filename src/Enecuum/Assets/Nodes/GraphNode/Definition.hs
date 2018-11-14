{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Assets.Nodes.GraphNode.Definition where

import           Enecuum.Prelude
import qualified Data.Aeson as A

import           Enecuum.Config
import           Enecuum.Assets.Nodes.GraphNode.Transmitter (graphNodeTransmitter)
import           Enecuum.Assets.Nodes.GraphNode.Config

instance Node GraphNode where
    data NodeScenario GraphNode = Transmitter
        deriving (Show, Generic)
    getNodeScript Transmitter = graphNodeTransmitter

instance ToJSON   (NodeScenario GraphNode) where toJSON    = A.genericToJSON nodeConfigJsonOptions
instance FromJSON (NodeScenario GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
