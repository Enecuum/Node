module Enecuum.Assets.Nodes.TstNodes.PingPong.Messages where

import Enecuum.Prelude

data Ping = Ping Text
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Pong = Pong Int
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
