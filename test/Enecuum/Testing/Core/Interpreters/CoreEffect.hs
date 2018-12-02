module Enecuum.Testing.Core.Interpreters.CoreEffect where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language                         as L

import           Enecuum.Testing.Core.Interpreters.ControlFlow
import qualified Enecuum.Testing.Core.Interpreters.Logger      as Impl
import qualified Enecuum.Testing.Types                         as T
-- Using real implementation
import           Enecuum.Core.Random.Interpreter               (runERandomL)

-- | Interprets core effect container language.
interpretCoreEffectF :: T.LoggerRuntime -> L.CoreEffectF a -> IO a
interpretCoreEffectF loggerRt (L.EvalLogger      logger next) = next <$> Impl.runLoggerL loggerRt logger

interpretCoreEffectF _        (L.EvalRandom      eRnd   next) = next <$> runERandomL eRnd

interpretCoreEffectF _        (L.EvalControlFlow flow   next) = next <$> runControlFlow flow
interpretCoreEffectF _        L.EvalFileSystem{}              = error "EvalFileSystem not implemented"

-- | Runs core effect container language.
runCoreEffectL :: T.LoggerRuntime -> L.CoreEffectL a -> IO a
runCoreEffectL loggerRt = foldFree (interpretCoreEffectF loggerRt)
