{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Framework.Domain.Node where

import qualified Data.Aeson                          as J
import           Data.Aeson.Extra                    (noLensPrefixJsonConfig)
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
    } deriving (Show, Eq, Ord, Generic, Serialize)

data NodeAddress = NodeAddress
    { _nodeHost  :: Host
    , _nodePorts :: NodePorts
    , _nodeId    :: NodeId
    } deriving (Show, Eq, Ord, Generic)

data NodeStatus = NodeActing | NodeFinished
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data StopNode = StopNode deriving Read

instance ToJSON   NodePorts   where toJSON    = J.genericToJSON    noLensPrefixJsonConfig
instance FromJSON NodePorts   where parseJSON = J.genericParseJSON noLensPrefixJsonConfig
instance ToJSON   NodeAddress where toJSON    = J.genericToJSON    noLensPrefixJsonConfig
instance FromJSON NodeAddress where parseJSON = J.genericParseJSON noLensPrefixJsonConfig

-- This is used in tests only.
-- TODO: get rid of it.
newtype NodeID = NodeID Text
    deriving (Show, Eq)

-- | Common
data SuccessMsg = SuccessMsg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype IsDead = IsDead StringHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Network messages
data Ping = Ping
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Pong = Pong
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Stop = Stop
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
