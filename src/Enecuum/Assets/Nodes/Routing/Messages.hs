{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Enecuum.Assets.Nodes.Routing.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

type NodeId     = StringHash
type PrivateKay = Bool

-- TODO : NodePorts & NodeAddress => to module ???
data NodePorts = NodePorts
    { _udpPort :: D.PortNumber
    , _tcpPort :: D.PortNumber
    , _rpcPort :: D.PortNumber
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Serialize)
makeFieldsNoPrefix ''NodePorts

makeNodePorts1000 :: D.PortNumber -> NodePorts
makeNodePorts1000 port = NodePorts (port - 1000) port (port + 1000)

data NodeAddress = NodeAddress
    { _hostAddress  :: D.Host
    , _nodePorts    :: NodePorts
    , _nodeId       :: NodeId
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
makeFieldsNoPrefix ''NodeAddress

makeNodeAddress :: D.Host -> NodePorts -> NodeId -> NodeAddress
makeNodeAddress = NodeAddress

getUdpAddress :: NodeAddress -> D.Address
getUdpAddress nodeAddress' =
    D.Address (nodeAddress'^.hostAddress) (nodeAddress'^.nodePorts.udpPort)

getTcpAddress :: NodeAddress -> D.Address
getTcpAddress nodeAddress' =
    D.Address (nodeAddress'^.hostAddress) (nodeAddress'^.nodePorts.tcpPort)

getRpcAddress :: NodeAddress -> D.Address
getRpcAddress nodeAddress' =
    D.Address (nodeAddress'^.hostAddress) (nodeAddress'^.nodePorts.rpcPort)

data HelloToBn = HelloToBn
    { _nodePorts :: NodePorts
    , _nodeId    :: NodeId
    , _signature :: Bool 
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
makeFieldsNoPrefix ''HelloToBn

makeHelloToBn :: Applicative m => PrivateKay -> NodePorts -> NodeId -> m HelloToBn
makeHelloToBn _ nodePorts' nodeId' = pure $ HelloToBn nodePorts' nodeId' True

verifyHelloToBn :: HelloToBn -> Bool
verifyHelloToBn _ = True

data HelloToBnResponce = HelloToBnResponce
    { _hostAddress :: D.Host
    , _signature   :: Bool
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
makeFieldsNoPrefix ''HelloToBnResponce

makeHelloToBnResponce :: Applicative m => PrivateKay -> D.Host ->  m HelloToBnResponce
makeHelloToBnResponce _ host' = pure $ HelloToBnResponce host' True

verifyHelloToBnResponce :: HelloToBnResponce -> Bool
verifyHelloToBnResponce _ = True

data RoutingHello = RoutingHello
    { _nodeAddress  :: NodeAddress
    , _signature    :: Bool 
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
makeFieldsNoPrefix ''RoutingHello

makeRoutingHello :: Applicative m => PrivateKay -> NodeAddress -> m RoutingHello
makeRoutingHello _ nodeAddress' = pure $ RoutingHello nodeAddress' True

verifyRoutingHello :: RoutingHello -> Bool
verifyRoutingHello _ = True

newtype NextForYou = NextForYou D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SendMsgTo = SendMsgTo
    { _nodeId     :: NodeId
    , _timeToLive :: Int
    , _msg        :: Text
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''SendMsgTo