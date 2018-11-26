{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Assets.Nodes.GraphNode.Definition where

import qualified Data.Aeson      as A
import           Enecuum.Prelude

-- import           Enecuum.Assets.Nodes.GraphNode.GN (graphNode)
import           Enecuum.Config

-- data GraphNode = GraphNode
--     deriving (Show, Generic)
--
-- instance Node GraphNode where
--     data NodeScenario GraphNode = GN
--         deriving (Show, Generic)
--     getNodeScript GN = graphNode
--
-- instance ToJSON   GraphNode               where toJSON    = A.genericToJSON    nodeConfigJsonOptions
-- instance FromJSON GraphNode               where parseJSON = A.genericParseJSON nodeConfigJsonOptions
-- instance ToJSON   (NodeScenario GraphNode) where toJSON    = A.genericToJSON nodeConfigJsonOptions
-- instance FromJSON (NodeScenario GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
