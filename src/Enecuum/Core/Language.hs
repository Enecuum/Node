module Enecuum.Core.Language
  ( module X
  , CoreEffects
  ) where

import Enecuum.Prelude

import Enecuum.Core.Logger.Language as X
import Enecuum.Core.HGraph.Language as X

-- | Core effects represent general effects every scenario can make.
type CoreEffects =
  '[ LoggerL
   , SIO
   , Exc SomeException
   ]
