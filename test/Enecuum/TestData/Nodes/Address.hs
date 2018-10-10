module Enecuum.TestData.Nodes.Address where

import qualified Enecuum.Domain as D

bootNodeAddr, masterNode1Addr :: D.Address
bootNodeAddr = D.Address "0.0.0.0" 2000
masterNode1Addr = D.Address "0.0.0.1" 2000

networkNode1Addr, networkNode2Addr :: D.Address
networkNode1Addr = D.Address "0.0.0.2" 2000
networkNode2Addr = D.Address "0.0.0.3" 2000

networkNode3Addr, networkNode4Addr :: D.Address
networkNode3Addr = D.Address "0.0.0.4" 2000
networkNode4Addr = D.Address "0.0.0.5" 2000

bootNodeTag, masterNodeTag :: D.NodeTag
bootNodeTag = "bootNode"
masterNodeTag = "masterNode"
