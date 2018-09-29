module Enecuum.Assets.Nodes.Address where

import qualified Enecuum.Domain                as D

networkNode1Addr, networkNode2Addr :: D.Address
networkNode1Addr = D.Address "127.0.0.1" 2001
networkNode2Addr = D.Address "127.0.0.2" 2002
