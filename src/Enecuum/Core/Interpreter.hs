module Enecuum.Core.Interpreter where

import Enecuum.Prelude
import Enecuum.Core.Language
import Control.Monad.Free
import Enecuum.Core.Logger.Interpreter
import Enecuum.Core.Logger.Runtime
import Enecuum.Core.Lens as L
import Enecuum.Core.Runtime

interpretCoreEffectF :: CoreRuntime -> CoreEffectF a -> IO a
interpretCoreEffectF cr (EvalLogger msg next) = do
    runLoggerL (_loggerRuntime cr) msg
    pure $ next ()

-- | Runs the LoggerL language.
runCoreEffectF :: CoreRuntime -> Free CoreEffectF a -> IO a
runCoreEffectF cr = foldFree (interpretCoreEffectF cr)
