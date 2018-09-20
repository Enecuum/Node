{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Core.Testing.Runtime.Types where


import           Control.Concurrent.Chan
import           Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Types           as T
import           Enecuum.Prelude

-- | Logger runtime. Configure logger
data LoggerRuntimeMemory = LoggerRuntimeMemory
  { _messages     :: TVar [Text]
  }

-- | Logger runtime. Configure logger
data LoggerRuntimeFile = LoggerRuntimeFile
  { _loggerChan   :: Chan LogMessageSimple,
    _loggerConfig :: TVar T.LoggerConfig
  }

createLoggerRuntimeMemory :: IO LoggerRuntimeMemory
createLoggerRuntimeMemory = do
  mes <- newTVarIO []
  pure $ LoggerRuntimeMemory {_messages = mes }

createLoggerRuntimeFile :: Chan LogMessageSimple -> T.LoggerConfig -> IO LoggerRuntimeFile
createLoggerRuntimeFile logChan config = do
  configT <- newTVarIO config
  pure $ LoggerRuntimeFile { _loggerChan = logChan, _loggerConfig = configT }
