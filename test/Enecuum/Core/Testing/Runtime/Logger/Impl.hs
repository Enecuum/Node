module Enecuum.Core.Testing.Runtime.Logger.Impl where

import Enecuum.Prelude

import           Eff (Eff, handleRelay)

import qualified Enecuum.Core.Language                      as L
import qualified Enecuum.Core.Testing.Runtime.Lens          as RLens
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Core.Types.Logger as T
import Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Logger.Interpreter as I (interpretLoggerLBase, runLoggerL)


-- | Interprets a LoggerL language.
-- Just pushes the messages into the concurrent list-like storage.
interpretLoggerL
  :: LoggerRuntime
  -> L.LoggerL a
  -> Eff '[SIO, Exc SomeException] a
interpretLoggerL rt (L.LogMessage logLevel msg) =
  safeIO $ do
  atomically $ modifyTVar (rt ^. RLens.messages) (msg :)
  -- readTVarIO --atomically . readTVar
  lvl <- readTVarIO (rt ^. RLens.currentLevel)
  let logFilePath = (rt ^. RLens.logFilePath)
  I.interpretLoggerLBase $ L.SetConfigForLog lvl logFilePath
  I.interpretLoggerLBase $ L.LogMessage logLevel msg


-- | Runs the LoggerL language.
runLoggerL
  :: LoggerRuntime
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[SIO, Exc SomeException] a
runLoggerL rt = handleRelay pure ( (>>=) . interpretLoggerL rt )


loggerTestWithoutConfig :: IO ()
loggerTestWithoutConfig = runSafeIO . I.runLoggerL $ do
  -- setConfigForLog T.Debug "withoutConfig"
  loggerTest

loggerTestWithConfig :: T.LogLevel -> IO ()
loggerTestWithConfig level = do
  loggerRuntime <- createLoggerRuntime level
  runSafeIO $ runLoggerL loggerRuntime loggerTest



loggerTest = do
    logMessage T.Debug "Debug Msg"
    logMessage T.Info "Info Msg"
    logMessage T.Warning "Warning Msg"
    logMessage T.Error "Error Msg"
