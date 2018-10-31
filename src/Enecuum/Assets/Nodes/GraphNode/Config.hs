{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Assets.Nodes.GraphNode.Config where

import Enecuum.Prelude
import Enecuum.Config

data GraphNode = GraphNode
    deriving (Show, Generic, FromJSON)

instance NodeCfg GraphNode where
    data NodeConfig GraphNode = GraphNodeConfig
        { database :: FilePath
        }
        deriving (Show, Generic, FromJSON)

