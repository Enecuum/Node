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

createLoggerRuntime :: Bool -> T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime isConsoleLog (T.LoggerConfig fmt lvl filePath) =
  LoggerRuntime . Just <$> Impl.setupLogger isConsoleLog fmt filePath lvl

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime (Just hsLogger)) = Impl.teardownLogger hsLogger
clearLoggerRuntime _ = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()
