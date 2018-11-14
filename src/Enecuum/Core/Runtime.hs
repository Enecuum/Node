module Enecuum.Core.Runtime where

import Enecuum.Prelude

import qualified Enecuum.Core.Types as T
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl

-- | Runtime data for the concrete logger impl.
newtype LoggerRuntime = LoggerRuntime
    { _hsLoggerHandle :: Maybe Impl.HsLoggerHandle
    }

-- | Runtime data for core subsystems.
newtype CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

-- | Logger that can be used in runtime via the logging subsystem.
newtype RuntimeLogger = RuntimeLogger
    { logMessage' :: T.LogLevel -> T.Message -> IO ()
    }

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = pure $ LoggerRuntime Nothing

createLoggerRuntime :: T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime config = LoggerRuntime . Just <$> Impl.setupLogger config

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime (Just hsLogger)) = Impl.teardownLogger hsLogger
clearLoggerRuntime _                               = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

mkRuntimeLogger :: LoggerRuntime -> RuntimeLogger
mkRuntimeLogger (LoggerRuntime hsLog) = RuntimeLogger
    { logMessage' = \lvl msg -> Impl.runLoggerL hsLog $ L.logMessage lvl msg
    }

-- Runtime log functions
logInfo' :: RuntimeLogger -> T.Message -> IO ()
logInfo' (RuntimeLogger l) = l T.Info

logError' :: RuntimeLogger -> T.Message -> IO ()
logError' (RuntimeLogger l) = l T.Error

logDebug' :: RuntimeLogger -> T.Message -> IO ()
logDebug' (RuntimeLogger l) = l T.Debug

logWarning' :: RuntimeLogger -> T.Message -> IO ()
logWarning' (RuntimeLogger l) = l T.Warning