module Enecuum.Core.CoreEffect.Language
  ( CoreEffectF (..)
  , CoreEffect
  , evalLogger
  ) where

import           Enecuum.Prelude

import           Enecuum.Core.Logger.Language (Logger, LoggerL, logMessage)
import Enecuum.Core.Random.Language

-- | Core effects container language.
data CoreEffectF next where
  -- | Logger effect
  EvalLogger :: LoggerL () -> (() -> next) -> CoreEffectF next
  EvalRandom :: RandomL g -> (a -> next) -> CoreEffectF next

instance Functor CoreEffectF where
  fmap g (EvalLogger logger next) = EvalLogger logger (g . next)
  fmap g (EvalRandom gen next) = EvalRandom gen (g . next)

type CoreEffect next = Free CoreEffectF next

evalLogger :: LoggerL () -> CoreEffect ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger (Free CoreEffectF) where
  logMessage level msg = evalLogger $ logMessage level msg

evalRandom :: RandomL a -> CoreEffect ()
evalRandom g = liftF $ EvalRandom g id

instance CRandom (Free CoreEffectF) where
  -- evalRand r = evalRandom $ evalRand r
  evalRand r = undefined
