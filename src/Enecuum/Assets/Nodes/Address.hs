module Enecuum.Assets.Nodes.Address where

import qualified Enecuum.Domain                as D

import           Enecuum.Legacy.Service.Network.Base (ConnectInfo (..))

networkNode1Addr, networkNode2Addr :: D.NodeAddress
networkNode1Addr = ConnectInfo "127.0.0.1" 1001
networkNode2Addr = ConnectInfo "127.0.0.2" 1002
