module Enecuum.Core.Runtime where

import Enecuum.Prelude

import qualified Enecuum.Core.Types as T
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl

data LoggerRuntime = LoggerRuntime
    { _hsLoggerHandle :: Maybe Impl.HsLoggerHandle
    }

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

-- TODO: make it right
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
