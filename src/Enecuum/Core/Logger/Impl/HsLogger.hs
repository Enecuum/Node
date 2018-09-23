module Enecuum.Core.Logger.Impl.HsLogger where

import           Enecuum.Prelude

import qualified Data.Text                   as TXT (unpack)
import           System.IO                   (Handle)
import           System.Log.Formatter
import           System.Log.Handler          (close, setFormatter)
import           System.Log.Handler.Simple   (GenericHandler, fileHandler)
import           System.Log.Logger

import qualified Enecuum.Core.Language       as L
import qualified Enecuum.Core.Types          as T (Format, LogLevel (..))

-- | Opaque type covering all information needed to teardown the logger.
data HsLoggerHandle = HsLoggerHandle
  { rootLogHandler :: GenericHandler Handle
  , logLock :: MVar Bool
  }

component :: String
component = "Node.Main"

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: Bool -> T.Format -> FilePath -> T.LogLevel -> (HsLoggerHandle -> IO c) -> IO c
withLogger isConsoleLog format logFileName level = bracket setupLogger' teardownLogger
  where setupLogger' = setupLogger isConsoleLog format logFileName level

-- | Dispatch log level from the LoggerL language
-- to the relevant log level of hslogger package
dispatchLogLevel :: T.LogLevel -> Priority
dispatchLogLevel T.Debug   = DEBUG
dispatchLogLevel T.Info    = INFO
dispatchLogLevel T.Warning = WARNING
dispatchLogLevel T.Error   = ERROR

-- | Interpret LoggerL language.
interpretLoggerL :: HsLoggerHandle -> L.LoggerF a -> IO a
interpretLoggerL h (L.LogMessage level msg next) = do
  -- TODO: Quick hack to log to console.
  isConsoleLog <- takeMVar $ logLock h
  when isConsoleLog $ print msg
  logM component (dispatchLogLevel level) $ TXT.unpack msg
  putMVar (logLock h) isConsoleLog
  pure $ next ()

runLoggerL :: Maybe HsLoggerHandle -> L.LoggerL () -> IO ()
runLoggerL (Just h) l = foldFree (interpretLoggerL h) l
runLoggerL Nothing _ = pure ()

-- | Setup logger required by the application.
setupLogger :: Bool -> T.Format -> FilePath -> T.LogLevel -> IO HsLoggerHandle
setupLogger isConsoleLog format logFileName level = do
  logHandler <- fileHandler logFileName (dispatchLogLevel level) >>=
        \lh -> pure $ setFormatter lh (simpleLogFormatter format)
  -- root Log
  updateGlobalLogger
      rootLoggerName
      (setLevel DEBUG . setHandlers [logHandler])
  -- return opaque AppLogger handle
  l <- newMVar isConsoleLog
  pure $ HsLoggerHandle logHandler l

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: HsLoggerHandle -> IO ()
teardownLogger = close . rootLogHandler
