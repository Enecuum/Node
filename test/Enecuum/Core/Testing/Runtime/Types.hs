module Enecuum.Core.Testing.Runtime.Types where

import Enecuum.Prelude

data LoggerRuntime = LoggerRuntime
  { _messages :: TVar [Text]
  }


createLoggerRuntime :: IO LoggerRuntime
createLoggerRuntime = LoggerRuntime <$> newTVarIO []
