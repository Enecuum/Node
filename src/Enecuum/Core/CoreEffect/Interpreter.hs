module Enecuum.Core.CoreEffect.Interpreter where

import           Enecuum.Prelude

import           Enecuum.Core.ControlFlow.Interpreter (runControlFlow)
import           Enecuum.Core.FileSystem.Interpreter  (runFileSystemL)
import qualified Enecuum.Core.Language                as L
import           Enecuum.Core.Logger.Impl.HsLogger    (runLoggerL)
import           Enecuum.Core.Random.Interpreter
import qualified Enecuum.Core.RLens                   as RLens
import           Enecuum.Core.Time.Interpreter        (runTimeL)
import qualified Enecuum.Runtime                      as Rt

-- | Interprets core effect.
interpretCoreEffectF :: Rt.CoreRuntime -> L.CoreEffectF a -> IO a
interpretCoreEffectF coreRt (L.EvalLogger msg next) =
    next <$> runLoggerL (coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle) msg
interpretCoreEffectF _      (L.EvalFileSystem s next)     = next <$> runFileSystemL s
interpretCoreEffectF _      (L.EvalRandom  s next)        = next <$> runERandomL s
interpretCoreEffectF coreRt (L.EvalControlFlow f    next) = next <$> runControlFlow coreRt f
interpretCoreEffectF coreRt (L.EvalTime f    next)        = next <$> runTimeL f
interpretCoreEffectF _      (L.EvalIO f next)             = next <$> f

-- | Runs core effect language.
runCoreEffectL :: Rt.CoreRuntime -> L.CoreEffectL a -> IO a
runCoreEffectL coreRt = foldFree (interpretCoreEffectF coreRt)
