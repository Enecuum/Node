module Enecuum.Core.Language
  ( module X
  , CoreEffects
  ) where

import Enecuum.Prelude

import Enecuum.Core.Logger.Language as X

type CoreEffects =
  '[ LoggerL
   , SIO
   , Exc SomeException
   ]
