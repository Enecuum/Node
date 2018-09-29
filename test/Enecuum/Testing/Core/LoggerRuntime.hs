module Enecuum.Testing.Core.LoggerRuntime where

import           Enecuum.Prelude

import qualified Enecuum.Testing.Types as T

createLoggerRuntime :: IO T.LoggerRuntime
createLoggerRuntime = T.LoggerRuntime <$> newTVarIO []
