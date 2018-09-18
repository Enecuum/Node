module Enecuum.Core.Logger.Hslogger where


import           Control.Exception             (bracket)
import           Enecuum.Core.System.Directory (defaultLogFileName)
import           Prelude
import           System.IO                     (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler            (LogHandler, close, setFormatter)
import           System.Log.Handler.Simple     (GenericHandler, fileHandler,
                                                streamHandler)
import           System.Log.Logger
                                           -- (Priority (..), rootLoggerName,
                                           --  setHandlers, setLevel,
                                           --  updateGlobalLogger)
import           Enecuum.Core.Types.Logger     (standartFormat)
import           Enecuum.Core.Types.Logger     (Format)


-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle {
      rootLogHandler   :: GenericHandler Handle
    }

-- | Install the loggers required by the application.
setupLogger ::  IO LoggerHandle
setupLogger = do
  logFileName <- defaultLogFileName
  logHandler <- fileHandler logFileName DEBUG
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
withLoggerOriginal :: IO a -> IO a
withLoggerOriginal = bracket setupLogger teardownLogger . const

-- withLogger :: (LoggerHandle -> IO c) -> IO c
withLogger = bracket setupLogger teardownLogger

-- | Set common format for logs
-- setCommonFormatter :: LoggerHandle  -> LoggerHandle
setCommonFormatter x =
  let f = simpleLogFormatter standartFormat in
  setFormatter x f


-- | Install the loggers required by the application.
setupLoggerNew ::  Format -> FilePath -> Priority -> IO LoggerHandle
setupLoggerNew format logFileName level = do
  logHandler <- fileHandler logFileName level >>=
        \lh -> return $ setFormatter lh (simpleLogFormatter format)
  -- root Log
  updateGlobalLogger
      rootLoggerName
      (setLevel DEBUG . setHandlers [logHandler])
  -- return opaque AppLogger handle
  return $ LoggerHandle logHandler

withLoggerOriginalNew :: Format -> FilePath -> Priority -> IO c -> IO c
withLoggerOriginalNew format logFileName level = bracket setupLogger teardownLogger . const
  where setupLogger = setupLoggerNew format logFileName level
