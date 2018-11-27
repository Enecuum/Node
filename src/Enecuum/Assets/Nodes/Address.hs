{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enecuum.Assets.Nodes.Address where

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
defaultPoANodePorts = makeNodePorts1000 5200

defaultPoANodeAddress :: NodeAddress
defaultPoANodeAddress = makeAddressByPorts defaultPoANodePorts
