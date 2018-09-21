module Enecuum.Core.Runtime where

import Enecuum.Prelude

import Enecuum.Core.Logger.Runtime (LoggerRuntime, createLoggerRuntime)

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

createCoreRuntime :: IO CoreRuntime
createCoreRuntime = CoreRuntime <$> createLoggerRuntime
