module Enecuum.Core.Interpreter where

import Enecuum.Prelude
import Enecuum.Core.Language
import Control.Monad.Free
import Enecuum.Core.Logger.Interpreter

interpretCoreEffectF :: CoreEffectF a -> IO a
interpretCoreEffectF (EvalLogger msg next) = do
    runLoggerL msg
    pure $ next ()

-- | Runs the LoggerL language.
runCoreEffectF :: Free CoreEffectF a -> IO a
runCoreEffectF = foldFree interpretCoreEffectF
