{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Assets.Nodes.GraphNode.Definition where

import Enecuum.Prelude
import Enecuum.Config
import Enecuum.Assets.Nodes.GraphNode.Transmitter (graphNodeTransmitter)
import Enecuum.Assets.Nodes.GraphNode.Receiver (graphNodeReceiver)
import Enecuum.Assets.Nodes.GraphNode.Config

data GraphNode = GraphNode
    deriving (Show, Generic, FromJSON)

instance Node GraphNode where
    data NodeScenario GraphNode = Transmitter | Receiver
            deriving (Generic, FromJSON)
    type NodeConfig GraphNode = GraphNodeConfig
    parseConfig = tryParseConfig
    getScenario = scenario
    getNode Transmitter = S.graphNodeTransmitter
    getNode Receiver    = S.graphNodeReceiver

-- instance Node GraphNode where
--     data NodeScenario GraphNode = Transmitter | Receiver
--             deriving (Generic, FromJSON)
--     data NodeConfig   GraphNode = GraphNodeConfig
--             { node     :: GraphNode
--             , scenario :: NodeScenario GraphNode
--             , database :: FilePath
--             }
--             deriving (Generic, FromJSON)
--     parseConfig = tryParseConfig
--     getScenario = scenario
--     getNode Transmitter = S.graphNodeTransmitter
--     getNode Receiver    = S.graphNodeReceiver
