module Enecuum.Testing.Core.Interpreters.CoreEffect where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language                    as L

import qualified Enecuum.Testing.Core.Interpreters.Logger as Impl
import qualified Enecuum.Testing.Types                    as T
import           Enecuum.Testing.Core.Interpreters.ControlFlow
-- Using real implementation
import           Enecuum.Core.Random.Interpreter (runERandomL)

-- | Interprets core effect container language.
interpretCoreEffectL :: T.LoggerRuntime -> L.CoreEffectF a -> IO a
interpretCoreEffectL loggerRt (L.EvalLogger logger next) =
    next <$> Impl.runLoggerL loggerRt logger

interpretCoreEffectL _ (L.EvalRandom eRnd next) =
    next <$> runERandomL eRnd

interpretCoreEffectL _ (L.EvalControlFlow flow next) =
    next <$> runControlFlow flow

-- | Runs core effect container language.
runCoreEffect :: T.LoggerRuntime -> L.CoreEffect a -> IO a
runCoreEffect loggerRt = foldFree (interpretCoreEffectL loggerRt)
