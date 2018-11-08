module Enecuum.Core.Types.State where

import           Data.HGraph.StringHashable (StringHash)

type VarId = StringHash

newtype StateVar a = StateVar
    { _varId :: VarId }
