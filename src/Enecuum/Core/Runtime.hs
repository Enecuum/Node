module Enecuum.Core.Runtime where

import Enecuum.Core.Logger.Runtime

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }


makeCoreRuntime :: CoreRuntime
makeCoreRuntime = CoreRuntime makeLoggerRuntime