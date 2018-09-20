{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Core.Testing.Runtime.Types where


import qualified Enecuum.Core.Types as T
import           Enecuum.Prelude


-- | Logger runtime. Configure logger
data LoggerRuntimeMemory = LoggerRuntimeMemory
  { _messages     :: TVar [Text]
  }

-- | Logger runtime. Configure logger
data LoggerRuntimeFile = LoggerRuntimeFile
  { _loggerConfig :: TVar T.LoggerConfig
  }

createLoggerRuntimeMemory :: IO LoggerRuntimeMemory
createLoggerRuntimeMemory = do
  mes <- newTVarIO []
  pure $ LoggerRuntimeMemory {_messages = mes }

createLoggerRuntimeFile :: T.LoggerConfig -> IO LoggerRuntimeFile
createLoggerRuntimeFile config = do
  configT <- newTVarIO config
  pure $ LoggerRuntimeFile { _loggerConfig = configT }
