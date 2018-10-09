module Enecuum.Framework.Domain.State where

import           Enecuum.Core.Types (StringHash)

type VarId = StringHash

newtype StateVar a = StateVar
    { _varId :: VarId }
