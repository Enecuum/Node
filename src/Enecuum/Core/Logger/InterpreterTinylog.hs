module Enecuum.Core.Logger.InterpreterTinylog where


import           Eff                          (Eff, handleRelay)
import qualified Enecuum.Core.Language        as L
import Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Types.Logger    as T
import           Enecuum.Prelude
import  System.Logger
import           Enecuum.Core.System.Directory (defaultLogFileName)

-- | Dispatch log level from the LoggerL language
-- to the relevant log level of tinylog package
dispatchLogLevel :: T.LogLevel -> Level
dispatchLogLevel T.Debug   = Debug
dispatchLogLevel T.Info    = Info
dispatchLogLevel T.Warning = Warn
dispatchLogLevel T.Error   = Error


-- | Interprets a LoggerL language.
-- via package tinylog
interpretLoggerL :: L.LoggerL a -> Eff '[SIO, Exc SomeException] a
interpretLoggerL (L.LogMessage logLevel message ) = safeIO $ do
  -- logger <- new defSettings
  logFileName <- defaultLogFileName
  -- logger <- new . setOutput StdOut . setFormat Nothing . setBufSize 0 $ defSettings
  logger <- new . setOutput (Path logFileName) . setFormat Nothing $ defSettings
  -- let logger = loggerDef {output = Path logFileName}
  System.Logger.log logger (dispatchLogLevel logLevel) $ msg message
interpretLoggerL (L.SetConfigForLog logLevel logFilename format) = safeIO $ do
  logger <- new defSettings
  print "hello"
  -- System.Logger.log logger (dispatchLogLevel logLevel) $ msg message

-- | Runs the LoggerL language.
runLoggerL
    :: Eff '[L.LoggerL, SIO, Exc SomeException] a
    -> Eff '[SIO, Exc SomeException] a
runLoggerL = handleRelay pure ((>>=) . interpretLoggerL )


go :: IO ()
go = runSafeIO . runLoggerL $ loggerTest


loggerTest = do
    logMessage T.Debug "Debug Msg"
    logMessage T.Info "Info Msg"
    logMessage T.Warning "Warning Msg"
    logMessage T.Error "Error Msg"
