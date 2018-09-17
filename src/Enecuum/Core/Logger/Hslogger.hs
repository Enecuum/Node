module Enecuum.Core.Logger.Hslogger where


import           Control.Exception             (bracket)
import           Enecuum.Core.System.Directory (defaultLogFileName)
import           Prelude
import           System.IO                     (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler            (close, setFormatter)
import           System.Log.Handler.Simple     (GenericHandler, fileHandler,
                                                streamHandler)
import           System.Log.Logger
                                           -- (Priority (..), rootLoggerName,
                                           --  setHandlers, setLevel,
                                           --  updateGlobalLogger)


-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle {
      rootLogHandler   :: GenericHandler Handle
    }


-- | Install the loggers required by the application.
setupLogger ::  FilePath -> IO LoggerHandle
setupLogger logFilePath = do
    appLog <- fileHandler logFilePath DEBUG

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel DEBUG . setHandlers [appLog])

    -- return opaque AppLogger handle
    return $ LoggerHandle appLog

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: LoggerHandle -> IO ()
teardownLogger handle = do
    close $ rootLogHandler handle

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: FilePath -> IO a -> IO a
withLogger logFilePath = bracket (setupLogger logFilePath) teardownLogger . const


-- | Set common format for logs
setCommonFormatter x =
  let f = simpleLogFormatter "$utcTime $prio $loggername: $msg" in
  setFormatter x f
