module Enecuum.Assets.Nodes.Address where

import qualified Enecuum.Domain                as D

localhost :: D.Host
localhost = "127.0.0.1"

graphNodeTransmitterRpcPort, graphNodeTransmitterTcpPort :: D.PortNumber
graphNodeTransmitterRpcPort = 2008
graphNodeTransmitterTcpPort = 3001

graphNodeReceiverRpcPort :: D.PortNumber
graphNodeReceiverRpcPort    = 2009

graphNodeTransmitterRpcAddress, graphNodeTransmitterTcpAddress :: D.Address
graphNodeTransmitterRpcAddress = D.Address "127.0.0.1" graphNodeTransmitterRpcPort
graphNodeTransmitterTcpAddress = D.Address "127.0.0.1" graphNodeTransmitterTcpPort

graphNodeReceiverRpcAddress :: D.Address
graphNodeReceiverRpcAddress = D.Address "127.0.0.1" graphNodeReceiverRpcPort


powNodeRpcPort :: D.PortNumber
powNodeRpcPort = 2005

powNodeRpcAddress :: D.Address
powNodeRpcAddress = D.Address "127.0.0.1" powNodeRpcPort

poaNodePort :: D.PortNumber
poaNodePort = 2006

poaNodeAddress :: D.Address
poaNodeAddress = D.Address "127.0.0.1" poaNodePort

poaNodeRpcPort :: D.PortNumber
poaNodeRpcPort = 2007

poaNodeRpcAddress :: D.Address
poaNodeRpcAddress = D.Address "127.0.0.1" poaNodeRpcPort

clientAddress :: D.Address
clientAddress = D.Address localhost clientRpcPort

clientRpcPort :: D.PortNumber
clientRpcPort = 2010

bnNodePort :: D.PortNumber
bnNodePort = 5000

bnAddress :: D.Address
bnAddress = D.Address "127.0.0.1" bnNodePort