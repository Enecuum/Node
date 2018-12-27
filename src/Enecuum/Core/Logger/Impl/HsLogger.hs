module Enecuum.Core.Logger.Impl.HsLogger where

import           Enecuum.Prelude

import qualified Data.Text                   as TXT (unpack)
import           System.IO                   (Handle, stdout)
import           System.Log.Formatter
import           System.Log.Handler          (close, setFormatter)
import           System.Log.Handler.Simple   (GenericHandler, fileHandler, streamHandler)
import           System.Log.Logger

import qualified Enecuum.Core.Language       as L
import qualified Enecuum.Core.Types          as T (LogLevel (..), LoggerConfig(..))

-- | Opaque type covering all information needed to teardown the logger.
data HsLoggerHandle = HsLoggerHandle
  { handlers :: [GenericHandler Handle]
  }

component :: String
component = ""

-- | Bracket an IO action which denotes the whole scope where the loggers of
-- the application are needed to installed. Sets them up before running the action
-- and tears them down afterwards. Even in case of an exception.
withLogger :: T.LoggerConfig -> (HsLoggerHandle -> IO c) -> IO c
withLogger config = bracket (setupLogger config) teardownLogger 

-- | Dispatch log level from the LoggerL language
-- to the relevant log level of hslogger package
dispatchLogLevel :: T.LogLevel -> Priority
dispatchLogLevel T.Debug   = DEBUG
dispatchLogLevel T.Info    = INFO
dispatchLogLevel T.Warning = WARNING
dispatchLogLevel T.Error   = ERROR

-- | Interpret LoggerL language.
interpretLoggerL :: HsLoggerHandle -> L.LoggerF a -> IO a
interpretLoggerL _ (L.LogMessage level msg next) = do
    logM component (dispatchLogLevel level) $ TXT.unpack msg
    pure $ next ()

runLoggerL :: Maybe HsLoggerHandle -> L.LoggerL () -> IO ()
runLoggerL (Just h) l = foldFree (interpretLoggerL h) l
runLoggerL Nothing  _ = pure ()

-- | Setup logger required by the application.
setupLogger :: T.LoggerConfig -> IO HsLoggerHandle
setupLogger (T.LoggerConfig format level logFileName isConsoleLog isFileLog) = do
    let logLevel     = dispatchLogLevel level
    let setFormat lh = pure $ setFormatter lh (simpleLogFormatter format)

    let fileH        = [fileHandler logFileName logLevel >>= setFormat | isFileLog   ]
    let consoleH     = [streamHandler stdout logLevel    >>= setFormat | isConsoleLog]

    handlers <- sequence $ fileH ++ consoleH

    when (length handlers > 0) $ updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers handlers)
    pure $ HsLoggerHandle handlers

-- TODO: FIXME: these clearings don't work for console logger.
-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger :: HsLoggerHandle -> IO ()
teardownLogger (HsLoggerHandle handlers) = do
    let x = setHandlers @(GenericHandler Handle) []
    updateGlobalLogger rootLoggerName (setLevel EMERGENCY . x)
    mapM_ close handlers

