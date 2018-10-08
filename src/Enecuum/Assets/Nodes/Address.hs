module Enecuum.Assets.Nodes.Address where

import qualified Enecuum.Domain                as D

networkNode1Addr, networkNode2Addr,
  networkNode3Addr, networkNode4Addr :: D.Address
networkNode1Addr = D.Address "127.0.0.1" 2001
networkNode2Addr = D.Address "127.0.0.2" 2002
networkNode3Addr = D.Address "127.0.0.1" 2003
networkNode4Addr = D.Address "127.0.0.1" 2004
nnAddr = D.Address "127.0.0.1" 2007

graphNodeRpcPort :: D.PortNumber
graphNodeRpcPort = 2008

graphNodeRpcAddress :: D.Address
graphNodeRpcAddress = D.Address "127.0.0.1" graphNodeRpcPort

powNodeRpcPort :: D.PortNumber
powNodeRpcPort = 2005

powNodeRpcAddress :: D.Address
powNodeRpcAddress = D.Address "127.0.0.1" powNodeRpcPort

poaNodePort :: D.PortNumber
poaNodePort = 2006

poaNodeAddress :: D.Address
poaNodeAddress = D.Address "127.0.0.1" poaNodePort