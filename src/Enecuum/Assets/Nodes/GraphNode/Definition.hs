{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Assets.Nodes.GraphNode.Definition where

import           Enecuum.Prelude
import qualified Data.Aeson as A

import           Enecuum.Config
import           Enecuum.Assets.Nodes.GraphNode.Transmitter (graphNodeTransmitter)
import           Enecuum.Assets.Nodes.GraphNode.Receiver (graphNodeReceiver)
import           Enecuum.Assets.Nodes.GraphNode.Config

instance Node GraphNode where
    data NodeScenario GraphNode = Transmitter | Receiver
        deriving (Show, Generic)
    getNodeScript Transmitter = graphNodeTransmitter
    getNodeScript Receiver    = graphNodeReceiver

instance ToJSON   (NodeScenario GraphNode) where toJSON    = A.genericToJSON nodeConfigJsonOptions
instance FromJSON (NodeScenario GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
