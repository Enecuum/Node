module Enecuum.Core.Logger.FSLogger where


import           Eff                          (Eff, handleRelay)
import qualified Enecuum.Core.Language        as L
import qualified Enecuum.Core.Types.Logger    as T
import           Enecuum.Prelude
import           System.Logger


dispatchLogLevel :: T.LogLevel -> Level
dispatchLogLevel T.Debug   = Debug
dispatchLogLevel T.Info    = Info
dispatchLogLevel T.Warning = Warn
dispatchLogLevel T.Error   = Error


-- dispatchMsg :: T.LogLevel -> String -> String -> IO ()
-- dispatchMsg :: T.LogLevel -> Logger -> (Msg -> Msg) -> IO ()
dispatchMsg T.Debug   = debug
dispatchMsg T.Info    = info
dispatchMsg T.Warning = warn
dispatchMsg T.Error   = err



-- -- | Interprets a LoggerL language.
-- -- via package hslogger
interpretLoggerL :: L.LoggerL a -> Eff '[SIO, Exc SomeException] a
interpretLoggerL (L.LogMessage logLevel message ) = safeIO $ do
  logger <- new defSettings
  (dispatchMsg logLevel) logger $ msg message
-- interpretLoggerL (SendTo filePath logLevel ) = safeIO $ do
--   fh <- fileHandler filePath $ dispatchLogLevel logLevel
--   updateGlobalLogger comp $ addHandler fh




-- -- | Runs the LoggerL language.
-- runLoggerL
--     :: Eff '[L.LoggerL, SIO, Exc SomeException] a
--     -> Eff '[SIO, Exc SomeException] a
-- runLoggerL = handleRelay pure ((>>=) . interpretLoggerL )
