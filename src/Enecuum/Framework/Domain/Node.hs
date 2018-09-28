module Enecuum.Framework.Domain.Node where

import           Enecuum.Prelude

import Enecuum.Framework.Domain.Networking ( NodeAddress )

-- Raw vision of node domain types.
-- Some of these types can be taken from Legacy.

type NodeTag = Text
newtype NodeID = NodeID Text
  deriving ( Show )

-- | Node address in the network.
newtype NodeConfig = NodeConfig
  { _address :: NodeAddress
  }
