module Enecuum.Core.Logger.Hslogger where


import           Control.Exception         (bracket)
import           Prelude
-- import System.IO(stderr)
import           System.IO                 (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler        (close, setFormatter)
import           System.Log.Handler.Simple (GenericHandler, fileHandler,
                                            streamHandler)
import           System.Log.Logger
                                           -- (Priority (..), rootLoggerName,
                                           --  setHandlers, setLevel,
                                           --  updateGlobalLogger)


-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle {
      rootLogHandler   :: GenericHandler Handle
    -- , accessLogHandler :: GenericHandler Handle
    , serverLogHandler :: GenericHandler Handle
    }

-- | Set common format for logs
setCommonFormatter x =
  let f = simpleLogFormatter "$utcTime $prio $loggername: $msg" in
  setFormatter x f


-- | Install the loggers required by the application.
setupLogger ::  FilePath -> IO LoggerHandle
setupLogger logFilePath = do
    appLog <- fileHandler logFilePath DEBUG
    -- accessLog <- fileHandler "access.log" DEBUG
    stdoutLog <- streamHandler stdout DEBUG

    -- let appLog' = setCommonFormatter appLog
    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel DEBUG . setHandlers [appLog])

    -- Access Log
    -- updateGlobalLogger
    --     comp1
    --     (setLevel DEBUG . setHandlers [accessLog])

    -- Server Log
    updateGlobalLogger
        comp2
        (setLevel DEBUG . setHandlers [stdoutLog])
    -- return opaque AppLogger handle
    return $ LoggerHandle appLog stdoutLog

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: LoggerHandle -> IO ()
teardownLogger handle = do
    close $ serverLogHandler handle
    -- close $ accessLogHandler handle
    close $ rootLogHandler   handle

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: FilePath -> IO a -> IO a
withLogger logFilePath = bracket (setupLogger logFilePath) teardownLogger . const


appFilename :: FilePath
appFilename = "app.log"

-- 'comp' is short for 'component'
comp1, comp2 :: String
comp1 = "Node.Comp1"
comp2 = "Node.Comp2"
