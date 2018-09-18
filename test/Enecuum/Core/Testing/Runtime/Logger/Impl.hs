module Enecuum.Core.Testing.Runtime.Logger.Impl where

import Enecuum.Prelude

import           Eff (Eff, handleRelay)

import qualified Enecuum.Core.Language                      as L
import qualified Enecuum.Core.Testing.Runtime.Lens          as RLens
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Core.Types.Logger as T
import Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Logger.InterpreterHslogger as H (interpretLoggerLBase, runLoggerL, interpretLoggerLBaseNew)


-- | Interprets a LoggerL language
-- via package hslogger.
interpretLoggerL
  :: LoggerRuntime
  -> L.LoggerL a
  -> Eff '[SIO, Exc SomeException] a
interpretLoggerL rt (L.LogMessage logLevel msg) =
  safeIO $ do
  atomically $ modifyTVar (rt ^. RLens.messages) (msg :)
  fileLevel <- readTVarIO (rt ^. RLens.currentLevel)
  let logFilePath = (rt ^. RLens.logFilePath)
      format = (rt ^. RLens.currentFormat)
  -- H.interpretLoggerLBase $ L.SetConfigForLog fileLevel logFilePath format
  -- H.interpretLoggerLBase $ L.LogMessage logLevel msg
  H.interpretLoggerLBaseNew $ L.LogMessageNew fileLevel logFilePath format logLevel msg


-- | Runs the LoggerL language
-- via package hslogger
runLoggerL
  :: LoggerRuntime
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[SIO, Exc SomeException] a
runLoggerL rt = handleRelay pure ( (>>=) . interpretLoggerL rt )


loggerTestWithoutConfig :: IO ()
loggerTestWithoutConfig = runSafeIO . H.runLoggerL $ do
  -- setConfigForLog T.Debug "withoutConfig"
  loggerTest


loggerTestSet :: T.LogLevel -> T.Format -> FilePath -> IO ()
loggerTestSet level format filePath = do
  loggerRuntime <- createLoggerRuntime level format filePath
  runSafeIO $ runLoggerL loggerRuntime loggerTest


loggerTest = do
    logMessage T.Debug "Debug Msg"
    logMessage T.Info "Info Msg"
    logMessage T.Warning "Warning Msg"
    logMessage T.Error "Error Msg"
