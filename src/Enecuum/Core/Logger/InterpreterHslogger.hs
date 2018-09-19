module Enecuum.Core.Logger.InterpreterHslogger where

import           Control.Exception         (bracket)
import qualified Data.Text                 as TXT (unpack)
import qualified Enecuum.Core.Language     as L
import           Enecuum.Core.Types.Logger
import           Prelude
import           System.IO                 (Handle)
import           System.Log.Formatter
import           System.Log.Handler        (close, setFormatter)
import           System.Log.Handler.Simple (GenericHandler, fileHandler)
import           System.Log.Logger

-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle {
      rootLogHandler   :: GenericHandler Handle
    }


-- | Setup logger required by the application.
setupLogger::  Format -> FilePath -> Priority -> IO LoggerHandle
setupLogger format logFileName level = do
  logHandler <- fileHandler logFileName level >>=
        \lh -> return $ setFormatter lh (simpleLogFormatter format)
  -- root Log
  updateGlobalLogger
      rootLoggerName
      (setLevel DEBUG . setHandlers [logHandler])
  -- return opaque AppLogger handle
  return $ LoggerHandle logHandler


-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: LoggerHandle -> IO ()
teardownLogger handle = do
    close $ rootLogHandler handle


-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: Format -> FilePath -> Priority -> IO c -> IO c
withLogger format logFileName level = bracket setupLoggerNew teardownLogger . const
  where setupLoggerNew = setupLogger format logFileName level


-- | Dispatch log level from the LoggerL language
-- to the relevant log level of hslogger package
dispatchLogLevel :: LogLevel -> Priority
dispatchLogLevel Debug   = DEBUG
dispatchLogLevel Info    = INFO
dispatchLogLevel Warning = WARNING
dispatchLogLevel Error   = ERROR


-- | Base primitive for the LoggerL language.
interpretLoggerL ::  L.LoggerL a -> IO ()
interpretLoggerL (L.LogMessageWithConfig fileLevel logFilename format logLevel msg ) = do
  withLogger format logFilename (dispatchLogLevel fileLevel) $
    logM comp (dispatchLogLevel logLevel) $ TXT.unpack msg
interpretLoggerL s = error $ "Expect only LogMessageWithConfig but get: " ++ show s


-- | 'comp' is short for 'component'
comp :: String
comp = "LoggingExample.Main"
