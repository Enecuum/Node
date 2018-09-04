module Enecuum.Framework.Testing.Runtime.STM where

import           Enecuum.Prelude

import           Enecuum.Framework.Testing.Runtime.Types
import qualified Enecuum.Framework.Testing.Runtime.Lens as RLens



setNodeTag rt = writeTVar (rt ^. RLens.nodeTag)
