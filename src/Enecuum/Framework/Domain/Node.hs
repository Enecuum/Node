{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Framework.Domain.Node where

import           Enecuum.Core.Types                  (StringHash)
import           Enecuum.Framework.Domain.Networking
import           Enecuum.Prelude
import           Network.Socket                      (PortNumber)

type NodeTag = Text
type NodeId = StringHash

data NodePorts = NodePorts
    { _nodeUdpPort :: PortNumber
    , _nodeTcpPort :: PortNumber
    , _nodeRpcPort :: PortNumber
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Serialize)

data NodeAddress = NodeAddress
    { _nodeHost  :: Host
    , _nodePorts :: NodePorts
    , _nodeId    :: NodeId
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data NodeStatus = NodeActing | NodeFinished
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data StopNode = StopNode deriving Read
