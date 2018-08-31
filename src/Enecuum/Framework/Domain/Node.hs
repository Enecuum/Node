module Enecuum.Framework.Domain.Node where

import Data.Text (Text)

-- Some of these types can be taken from Legacy.

type NodeTag = Text

data NodeConfig = NodeConfig
  {

  }

newtype NodeID = NodeID Text
