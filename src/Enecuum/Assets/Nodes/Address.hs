module Enecuum.Assets.Nodes.Address where

import qualified Enecuum.Domain                as D

networkNode1Addr, networkNode2Addr,
  networkNode3Addr, networkNode4Addr :: D.Address
graphNodeAddr    = D.Address "127.0.0.1" 2001
networkNode1Addr = D.Address "127.0.0.1" 2001
networkNode2Addr = D.Address "127.0.0.2" 2002
networkNode3Addr = D.Address "127.0.0.1" 2003
networkNode4Addr = D.Address "127.0.0.1" 2004
powAddr = D.Address "127.0.0.1" 2005
poaAddr = D.Address "127.0.0.1" 2006
nnAddr = D.Address "127.0.0.1" 2007

graphNodeRpcPort :: D.PortNumber
graphNodeRpcPort = 2008

grpahNodeRpcAddress :: D.Address
grpahNodeRpcAddress = D.Address "127.0.0.1" graphNodeRpcPort
