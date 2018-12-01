module Enecuum.Core.Types.State where

import           Enecuum.Prelude
import           Data.HGraph.StringHashable (StringHash)

type VarId = StringHash

-- | Concurrent variable (STM TVar).
newtype StateVar a = StateVar
    { _varId :: VarId }

-- | Denotes a signaling concurrent variable.
type SignalVar = StateVar Bool
