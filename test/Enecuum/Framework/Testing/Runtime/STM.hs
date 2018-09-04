module Enecuum.Framework.Testing.Runtime.STM where

import           Enecuum.Prelude

import qualified Enecuum.Domain                          as D
import           Enecuum.Framework.Testing.Runtime.Types
import qualified Enecuum.Framework.Testing.Runtime.Lens  as RLens



setNodeTag :: NodeRuntime -> D.NodeTag -> STM ()
setNodeTag rt = writeTVar (rt ^. RLens.tag)
