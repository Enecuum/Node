module Enecuum.Core.Testing.Runtime.Logger.ImplFile where

import           Enecuum.Prelude

import           Control.Monad.Free                      (foldFree)

import           Control.Concurrent.Chan
import qualified Enecuum.Core.Language                   as L
import qualified Enecuum.Core.Logger.InterpreterHslogger as H (interpretLoggerL)
import           Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Testing.Runtime.Lens       as RLens
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Core.Types.Logger               as T


-- | Interprets a LoggerL language
-- via package hslogger.
interpretLoggerL :: LoggerRuntimeFile -> L.LoggerF a -> IO a
interpretLoggerL rt (L.LogMessage logLevel msg next) = do
  let loggerChan = (rt ^. RLens.loggerChan)
  writeChan loggerChan (LogMessageSimple logLevel msg)

  config <- readTVarIO (rt ^. RLens.loggerConfig)
  let (T.LoggerConfig format fileLevel logFilePath) = config
  -- H.interpretLoggerL $ L.SetupFile fileLevel logFilePath format next
  H.interpretLoggerL $ L.LogMessage logLevel msg next
  -- pure $ next ()

-- | Runs the LoggerL language
-- save to file via package hslogger.
runLoggerL :: LoggerRuntimeFile -> L.LoggerL a -> IO a
runLoggerL loggerRt = foldFree (interpretLoggerL loggerRt)
