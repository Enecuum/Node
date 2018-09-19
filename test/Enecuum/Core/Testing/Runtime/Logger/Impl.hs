module Enecuum.Core.Testing.Runtime.Logger.Impl where

import           Enecuum.Prelude

import           Control.Monad.Free                      (foldFree)

import qualified Enecuum.Core.Language                   as L
import qualified Enecuum.Core.Logger.InterpreterHslogger as H (interpretLoggerL)
import           Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Testing.Runtime.Lens       as RLens
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Core.Types.Logger               as T


-- | Interprets a LoggerL language
-- via package hslogger.
interpretLoggerL :: LoggerRuntime -> L.LoggerF a -> IO a
interpretLoggerL rt (L.LogMessage logLevel msg next) = do
  atomically $ modifyTVar (rt ^. RLens.messages) (msg :)
  fileLevel <- readTVarIO (rt ^. RLens.currentLevel)
  let logFilePath = (rt ^. RLens.logFilePath)
      format      = (rt ^. RLens.currentFormat)
  H.interpretLoggerL $ L.LogMessageWithConfig fileLevel logFilePath format logLevel msg next

-- | Runs the LoggerL language
-- via package hslogger
runLoggerL :: LoggerRuntime -> L.LoggerL a -> IO a
runLoggerL loggerRt = foldFree (interpretLoggerL loggerRt)


-- | Initialize Logger Runtime and run test
loggerTestSet :: T.LogLevel -> T.Format -> FilePath -> IO ()
loggerTestSet level format filePath = do
  loggerRuntime <- createLoggerRuntime level format filePath
  runLoggerL loggerRuntime loggerTest


loggerTest = do
    logMessage T.Debug "Debug Msg"
    logMessage T.Info "Info Msg"
    logMessage T.Warning "Warning Msg"
    logMessage T.Error "Error Msg"

-- -- | Interprets a LoggerL language.
-- -- Just pushes the messages into the concurrent list-like storage.
-- interpretLoggerL :: LoggerRuntime -> L.LoggerF a -> IO a
-- interpretLoggerL loggerRt (L.LogMessage _ msg next) = do
--   atomically $ modifyTVar (loggerRt ^. RLens.messages) (msg :)
--   pure $ next ()
