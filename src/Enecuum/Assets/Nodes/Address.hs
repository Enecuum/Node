{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enecuum.Assets.Nodes.Address where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

type NodeId     = StringHash

-- TODO : NodePorts & NodeAddress => to module ???
data NodePorts = NodePorts
    { _nodeUdpPort :: D.PortNumber
    , _nodeTcpPort :: D.PortNumber
    , _nodeRpcPort :: D.PortNumber
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Serialize)
makeFieldsNoPrefix ''NodePorts

makeNodePorts1000 :: D.PortNumber -> NodePorts
makeNodePorts1000 port = NodePorts (port - 1000) port (port + 1000)

data NodeAddress = NodeAddress
    { _nodeHost     :: D.Host
    , _nodePorts    :: NodePorts
    , _nodeId       :: NodeId
    } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
makeFieldsNoPrefix ''NodeAddress

makeNodeAddress :: D.Host -> NodePorts -> NodeId -> NodeAddress
makeNodeAddress = NodeAddress

getUdpAddress :: NodeAddress -> D.Address
getUdpAddress nodeAddress' =
    D.Address (nodeAddress'^.nodeHost) (nodeAddress'^.nodePorts.nodeUdpPort)

getTcpAddress :: NodeAddress -> D.Address
getTcpAddress nodeAddress' =
    D.Address (nodeAddress'^.nodeHost) (nodeAddress'^.nodePorts.nodeTcpPort)

getRpcAddress :: NodeAddress -> D.Address
getRpcAddress nodeAddress' =
    D.Address (nodeAddress'^.nodeHost) (nodeAddress'^.nodePorts.nodeRpcPort)

localhost :: D.Host
localhost = "127.0.0.1"

makeAddressByPorts :: NodePorts -> NodeAddress 
makeAddressByPorts ports = NodeAddress localhost ports (D.toHashGeneric ports)

-- List of test and default port.
-- udp = nodePort - 1000
-- tcp = nodePort
-- rpc = nodePort + 1000

-- bn = [0 .. 9]
defaultBnNodePorts :: NodePorts
defaultBnNodePorts = makeNodePorts1000 5000

defaultBnNodeAddress :: NodeAddress
defaultBnNodeAddress = makeAddressByPorts defaultBnNodePorts

-- client = [10 .. 19]
defaultClientPorts :: NodePorts
defaultClientPorts = makeNodePorts1000 5010

defaultClientAddress :: NodeAddress
defaultClientAddress = makeAddressByPorts defaultClientPorts

-- pow = [20 .. 49]
defaultPoWNodePorts :: NodePorts
defaultPoWNodePorts = makeNodePorts1000 5020

defaultPoWNodeAddress :: NodeAddress
defaultPoWNodeAddress = makeAddressByPorts defaultPoWNodePorts

-- gn = [50 .. 199]
-- work gn node
defaultGnNodePorts :: NodePorts
defaultGnNodePorts = makeNodePorts1000 5050

defaultGnNodeAddress :: NodeAddress
defaultGnNodeAddress = makeAddressByPorts defaultGnNodePorts

-- test receiver node
defaultGnReceiverNodePorts :: NodePorts
defaultGnReceiverNodePorts = makeNodePorts1000 5051

defaultGnReceiverNodeAddress :: NodeAddress
defaultGnReceiverNodeAddress = makeAddressByPorts defaultGnReceiverNodePorts

-- poa = [200 .. 999]
defaultPoANodePorts :: NodePorts
defaultPoANodePorts = makeNodePorts1000 5201

defaultPoANodeAddress :: NodeAddress
defaultPoANodeAddress = makeAddressByPorts defaultPoANodePorts