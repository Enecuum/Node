module Enecuum.Core.Testing.Runtime.Types where

import           Enecuum.Core.System.Directory (appFileName)
import qualified Enecuum.Core.Types            as T
import           Enecuum.Prelude


-- type Formater = String

-- | Logger runtime. Configure logger
data LoggerRuntime = LoggerRuntime
  { _messages     :: TVar [Text]
  -- , currentFormater :: Formater
  , _currentLevel :: TVar T.LogLevel
  , _logFilePath  :: FilePath
  }


createLoggerRuntime :: T.LogLevel -> IO LoggerRuntime
createLoggerRuntime level = do
  mes <- newTVarIO []
  lvl <- newTVarIO level
  let appFilename = "appfun.log"
  pure $ LoggerRuntime {_messages = mes, _currentLevel = lvl, _logFilePath = appFilename}
