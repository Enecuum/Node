module Enecuum.Core.Logger.Interpreter where

import           Enecuum.Prelude
import           Eff (Eff, handleRelay)
import qualified Enecuum.Core.Language                      as L


-- | Interprets a LoggerL language.
-- Print a log msg in std out.
interpretLoggerL :: L.LoggerL a -> Eff '[SIO, Exc SomeException] a
interpretLoggerL (L.LogMessage _ msg) = safeIO $ putStrLn msg

-- | Runs the LoggerL language.
runLoggerL
    :: Eff '[L.LoggerL, SIO, Exc SomeException] a
    -> Eff '[SIO, Exc SomeException] a
runLoggerL = handleRelay pure ((>>=) . interpretLoggerL)

