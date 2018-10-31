{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Assets.Nodes.GraphNode.Config where

import Enecuum.Prelude

data GraphNodeConfig = GraphNodeConfig
    { database :: FilePath
    }
    deriving (Show, Generic, FromJSON)