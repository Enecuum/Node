module Enecuum.Testing.Core.Interpreters.CoreEffect where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language                    as L

import qualified Enecuum.Testing.Core.Interpreters.Logger as Impl
import qualified Enecuum.Testing.Types                    as T

-- | Interprets core effect container language.
interpretCoreEffectL :: T.LoggerRuntime -> L.CoreEffectF a -> IO a
interpretCoreEffectL loggerRt (L.EvalLogger logger next) =
    next <$> Impl.runLoggerL loggerRt logger

-- | Runs core effect container language.
runCoreEffect :: T.LoggerRuntime -> L.CoreEffect a -> IO a
runCoreEffect loggerRt = foldFree (interpretCoreEffectL loggerRt)
