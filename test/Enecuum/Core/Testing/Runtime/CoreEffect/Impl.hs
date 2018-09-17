module Enecuum.Core.Testing.Runtime.CoreEffect.Impl where

import Enecuum.Prelude

import           Control.Monad.Free                         (foldFree)

import qualified Enecuum.Core.Language                      as L
import qualified Enecuum.Core.Testing.Runtime.Lens          as RLens
import           Enecuum.Core.Testing.Runtime.Logger.Impl   (runLoggerL)
import           Enecuum.Core.Testing.Runtime.Types

-- | Interprets core effect container language.
interpretCoreEffectL :: LoggerRuntime -> L.CoreEffectF a -> IO a
interpretCoreEffectL loggerRt (L.EvalLogger logger next) =
    next <$> runLoggerL loggerRt logger

-- | Runs core effect container language.
runCoreEffectModel :: LoggerRuntime -> L.CoreEffectModel a -> IO a
runCoreEffectModel loggerRt = foldFree (interpretCoreEffectL loggerRt)
