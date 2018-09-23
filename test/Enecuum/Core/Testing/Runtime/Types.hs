{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Core.Testing.Runtime.Types where

import           Control.Concurrent.Chan
import           Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Types           as T
import           Enecuum.Prelude

-- | Logger runtime. Stores messages.
data LoggerRuntime = LoggerRuntime
  { _messages     :: TVar [Text]
  }

createLoggerRuntime :: IO LoggerRuntime
createLoggerRuntime = LoggerRuntime <$> newTVarIO []
