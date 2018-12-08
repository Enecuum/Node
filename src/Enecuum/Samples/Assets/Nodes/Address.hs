{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enecuum.Samples.Assets.Nodes.Address where

import           Data.HGraph.StringHashable
import           Enecuum.Domain             (Address (..), NodeAddress (..), NodeId, NodePorts (..))
import qualified Enecuum.Domain             as D
import qualified Enecuum.Framework.Lens     as Lens
import           Enecuum.Prelude

makeNodePorts1000 :: D.PortNumber -> NodePorts
makeNodePorts1000 port = NodePorts (port - 1000) port (port + 1000)

makeNodeAddress :: D.Host -> NodePorts -> NodeId -> NodeAddress
makeNodeAddress = NodeAddress

getUdpAddress :: NodeAddress -> Address
getUdpAddress nodeAddress' =
    D.Address (nodeAddress' ^. Lens.nodeHost) (nodeAddress' ^. Lens.nodePorts . Lens.nodeUdpPort)

getTcpAddress :: NodeAddress -> Address
getTcpAddress nodeAddress' =
    D.Address (nodeAddress' ^. Lens.nodeHost) (nodeAddress' ^. Lens.nodePorts . Lens.nodeTcpPort)

getRpcAddress :: NodeAddress -> Address
getRpcAddress nodeAddress' =
    D.Address (nodeAddress' ^. Lens.nodeHost) (nodeAddress' ^. Lens.nodePorts . Lens.nodeRpcPort)

localhost :: D.Host
localhost = "127.0.0.1"

makeAddressByPorts :: NodePorts -> NodeAddress
makeAddressByPorts ports = NodeAddress localhost ports (D.toHashGeneric ports)

-- Aggreement:
-- udp ports from 4000 to 4999
-- tcp ports from 5000 to 5999
-- rpc ports from 6000 to 6999

-- udp = nodePort - 1000
-- tcp = nodePort
-- rpc = nodePort + 1000

-- client = [10 .. 19]
clientPorts :: NodePorts
clientPorts = makeNodePorts1000 5010

clientAddress :: NodeAddress
clientAddress = makeAddressByPorts clientPorts

-- Test nodes
tstGenPoANodePorts :: NodePorts
tstGenPoANodePorts = makeNodePorts1000 5200

tstGenPoANodeAddress :: NodeAddress
tstGenPoANodeAddress = makeAddressByPorts tstGenPoANodePorts

tstGenPoWNodePorts :: NodePorts
tstGenPoWNodePorts = makeNodePorts1000 5020

tstGenPoWNodeAddress :: NodeAddress
tstGenPoWNodeAddress = makeAddressByPorts tstGenPoWNodePorts

tstGraphNodeTransmitterPorts :: NodePorts
tstGraphNodeTransmitterPorts = makeNodePorts1000 5050

tstGraphNodeTransmitterAddress :: NodeAddress
tstGraphNodeTransmitterAddress = makeAddressByPorts tstGraphNodeTransmitterPorts

tstGraphNodeReceiverPorts :: NodePorts
tstGraphNodeReceiverPorts = makeNodePorts1000 5051

tstGraphNodeReceiverAddress :: NodeAddress
tstGraphNodeReceiverAddress = makeAddressByPorts tstGraphNodeReceiverPorts
