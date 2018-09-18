module Enecuum.Core.Testing.Runtime.Types where

import           Enecuum.Core.System.Directory (appFileName)
import           Enecuum.Core.System.Directory (defaultLogFileName)
import qualified Enecuum.Core.Types            as T
import           Enecuum.Prelude


-- | Logger runtime. Configure logger
data LoggerRuntime = LoggerRuntime
  { _messages      :: TVar [Text]
  , _currentFormat :: T.Format
  , _currentLevel  :: TVar T.LogLevel
  , _logFilePath   :: FilePath
  }


createLoggerRuntime :: T.LogLevel -> T.Format -> FilePath -> IO LoggerRuntime
createLoggerRuntime level format logFile = do
  mes <- newTVarIO []
  lvl <- newTVarIO level
  pure $ LoggerRuntime {_messages = mes,
                        _currentLevel = lvl,
                        _currentFormat = format,
                        _logFilePath = logFile }
