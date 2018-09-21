module Enecuum.Core.CoreEffect.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Core.Language as L
import qualified Enecuum.Runtime as Rt
import qualified Enecuum.Core.RLens as RLens
import           Enecuum.Core.Logger.Impl.HsLogger (runLoggerL)

-- | Interprets core effect.
interpretCoreEffectF :: Rt.CoreRuntime -> L.CoreEffectF a -> IO a
interpretCoreEffectF coreRt (L.EvalLogger msg next) =
    next <$> runLoggerL msg

-- | Runs core effect language.
runCoreEffect :: Rt.CoreRuntime -> L.CoreEffect a -> IO a
runCoreEffect coreRt = foldFree (interpretCoreEffectF coreRt)
