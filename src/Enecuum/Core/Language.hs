module Enecuum.Core.Language
  ( module X
  , CoreEffectF (..)
  , CoreEffectModel
  , evalLogger
  ) where

import           Enecuum.Prelude

import           Control.Monad.Free           (Free (..), liftF)
import           Enecuum.Core.HGraph.Language as X
import           Enecuum.Core.Logger.Language as X
import qualified Enecuum.Core.Types           as T

-- | Core effects container language.
data CoreEffectF next where
  -- | Logger effect
  EvalLogger :: LoggerL () -> (() -> next) -> CoreEffectF next

instance Functor CoreEffectF where
  fmap g (EvalLogger logger next) = EvalLogger logger (g . next)

type CoreEffectModel next = Free CoreEffectF next

evalLogger :: LoggerL () -> CoreEffectModel ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger (Free CoreEffectF) where
  logMessage level msg = evalLogger $ logMessage level msg
