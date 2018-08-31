module Enecuum.Framework.Domain.NodeDefinition where

import Enecuum.Framework.Domain.Node (NodeID)
import Enecuum.Framework.Domain.Networking (ServerDef)

data NodeDef = NodeDef
  { nodeID :: NodeID
  , serverDef :: ServerDef
  }
