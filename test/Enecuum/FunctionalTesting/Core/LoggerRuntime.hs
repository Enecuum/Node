module Enecuum.FunctionalTesting.Core.LoggerRuntime where

import           Enecuum.Prelude

import qualified Enecuum.FunctionalTesting.Types as T

createLoggerRuntime :: IO T.LoggerRuntime
createLoggerRuntime = T.LoggerRuntime <$> newTVarIO []
