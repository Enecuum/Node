module Enecuum.Core.Testing.Runtime.CoreEffect.Impl where

import           Enecuum.Prelude

import           Control.Monad.Free                             (foldFree)

import qualified Enecuum.Core.Language                          as L
import           Enecuum.Core.Testing.Runtime.Logger.ImplMemory (runLoggerL)
import           Enecuum.Core.Testing.Runtime.Types             (LoggerRuntimeMemory)

-- | Interprets core effect container language.
interpretCoreEffectL :: LoggerRuntimeMemory -> L.CoreEffectF a -> IO a
interpretCoreEffectL loggerRt (L.EvalLogger logger next) =
    next <$> runLoggerL loggerRt logger

-- | Runs core effect container language.
runCoreEffectModel :: LoggerRuntimeMemory -> L.CoreEffectModel a -> IO a
runCoreEffectModel loggerRt = foldFree (interpretCoreEffectL loggerRt)
