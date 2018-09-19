module Enecuum.Framework.Domain.State where

import           Enecuum.Prelude

import           Enecuum.Core.Types (StringHash)

type VarId = StringHash

data StateVar a = StateVar VarId