module Enecuum.Core.Logger.Interpreter where

import           Enecuum.Prelude
import           Eff (Eff, handleRelay)
import qualified Enecuum.Core.Language                      as L
import System.Log (Priority(..))
import System.Log.Logger
import System.Log.Handler (close)
import System.Log.Handler.Simple
import Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Types.Logger as T
import qualified Data.Text as TXT (unpack)
import Enecuum.Core.Logger.Hslogger (withLogger)
import Enecuum.Core.System.Directory (appFilename)

dispatchLogLevel :: T.LogLevel -> Priority
dispatchLogLevel T.Debug   = DEBUG
dispatchLogLevel T.Info    = INFO
dispatchLogLevel T.Warning = WARNING
dispatchLogLevel T.Error   = ERROR


dispatchMsg :: T.LogLevel -> String -> String -> IO ()
dispatchMsg T.Debug   = debugM
dispatchMsg T.Info    = infoM
dispatchMsg T.Warning = warningM
dispatchMsg T.Error   = errorM


-- | Interprets a LoggerL language.
-- via package hslogger
interpretLoggerL :: L.LoggerL a -> Eff '[SIO, Exc SomeException] a
interpretLoggerL lo@(L.LogMessage logLevel msg ) = safeIO $ interpretLoggerLBase lo

-- | Base primitive for the LoggerL language.
interpretLoggerLBase ::  L.LoggerL a -> IO ()
interpretLoggerLBase (L.LogMessage logLevel msg ) =
  withLogger appFilename $ (dispatchMsg logLevel) comp $ TXT.unpack msg
interpretLoggerLBase (L.SetConfigForLog level logFilename) =
  withLogger logFilename $ updateGlobalLogger comp $ setLevel $ dispatchLogLevel level


-- | Runs the LoggerL language.
runLoggerL
    :: Eff '[L.LoggerL, SIO, Exc SomeException] a
    -> Eff '[SIO, Exc SomeException] a
runLoggerL = handleRelay pure ((>>=) . interpretLoggerL )


-- 'comp' is short for 'component'
comp :: String
comp = "LoggingExample.Main"
