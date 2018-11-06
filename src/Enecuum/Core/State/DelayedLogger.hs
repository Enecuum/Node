module Enecuum.Core.State.DelayedLogger where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language       as L
import qualified Enecuum.Core.Runtime        as Rt


-- | Interpret LoggerL language for a delayed log.
interpretDelayedLoggerF :: TVar Rt.DelayedLog -> L.LoggerF a -> STM a
interpretDelayedLoggerF delayedLog (L.LogMessage level msg next) =
    next <$> modifyTVar delayedLog (Rt.DelayedLogEntry level msg :)

-- | Interpret LoggerL language for a delayed log.
runDelayedLoggerL :: TVar Rt.DelayedLog -> L.LoggerL () -> STM ()
runDelayedLoggerL delayedLog = foldFree (interpretDelayedLoggerF delayedLog)