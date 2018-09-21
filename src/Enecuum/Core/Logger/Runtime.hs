module Enecuum.Core.Logger.Runtime where

import Enecuum.Prelude

-- Not used currently.
data LoggerRuntime = LoggerRuntime

createLoggerRuntime :: IO LoggerRuntime
createLoggerRuntime = pure LoggerRuntime
