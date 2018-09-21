module Enecuum.Core.CoreEffect.Language
  ( CoreEffectF (..)
  , CoreEffect
  , evalLogger
  ) where

import           Enecuum.Prelude

import           Enecuum.Core.Logger.Language (Logger, LoggerL, logMessage)
import qualified Enecuum.Core.Types           as T

-- | Core effects container language.
data CoreEffectF next where
  -- | Logger effect
  EvalLogger :: LoggerL () -> (() -> next) -> CoreEffectF next

instance Functor CoreEffectF where
  fmap g (EvalLogger logger next) = EvalLogger logger (g . next)

type CoreEffect next = Free CoreEffectF next

evalLogger :: LoggerL () -> CoreEffect ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger (Free CoreEffectF) where
  logMessage level msg = evalLogger $ logMessage level msg
