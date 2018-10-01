module Enecuum.Framework.Domain.Node where

import           Enecuum.Prelude

type NodeTag = Text

newtype NodeID = NodeID Text
  deriving ( Show )
