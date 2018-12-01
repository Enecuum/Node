{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Assets.Nodes.TstNodes.PingPong.Messages where

import           Enecuum.Prelude

newtype Ping = Ping Text
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Pong = Pong Int
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
