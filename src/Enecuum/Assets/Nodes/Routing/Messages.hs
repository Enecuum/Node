{-# LANGUAGE DuplicateRecordFields  #-}
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
    }
makeFieldsNoPrefix ''NodePorts

makeNodePorts1000 :: D.PortNumber -> NodePorts
makeNodePorts1000 port = NodePorts (port - 1000) port (port + 1000)

data NodeAddress = NodeAddress
    { _hostAddress  :: D.Host
    , _nodePorts    :: NodePorts
    , _nodeId       :: NodeId
    }
makeFieldsNoPrefix ''NodeAddress

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
    }
makeFieldsNoPrefix ''HelloToBn

makeHelloToBn :: Applicative m => PrivateKay -> NodePorts -> NodeId -> m HelloToBn
makeHelloToBn _ nodePorts' nodeId' = pure $ HelloToBn nodePorts' nodeId' True

verifyHelloToBn :: HelloToBn -> Bool
verifyHelloToBn _ = True

data HelloToBnResponce = HelloToBnResponce
    { _hostAddress :: D.Host
    , _signature   :: Bool
    }
makeFieldsNoPrefix ''HelloToBnResponce

makeHelloToBnResponce :: Applicative m => PrivateKay -> D.Host ->  m HelloToBnResponce
makeHelloToBnResponce _ host' = pure $ HelloToBnResponce host' True

verifyHelloToBnResponce :: HelloToBnResponce -> Bool
verifyHelloToBnResponce _ = True

data RoutingHello = RoutingHello
    { _nodeAddress  :: NodeAddress
    , _signature    :: Bool 
    }
makeFieldsNoPrefix ''RoutingHello

makeRoutingHello :: Applicative m => PrivateKay -> NodeAddress -> m RoutingHello
makeRoutingHello _ nodeAddress' = pure $ RoutingHello nodeAddress' True

verifyRoutingHello :: RoutingHello -> Bool
verifyRoutingHello _ = True


