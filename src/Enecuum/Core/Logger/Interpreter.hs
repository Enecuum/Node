module Enecuum.Core.Logger.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free
import qualified Enecuum.Core.Language                      as L
import           Enecuum.Core.Logger.Runtime


-- | Interprets a LoggerL language.
-- Print a log msg in std out.
interpretLoggerL :: LoggerRuntime -> L.LoggerF a -> IO a
interpretLoggerL _ (L.LogMessage _ msg next) = do
    putStrLn msg
    pure $ next ()

-- | Runs the LoggerL language.
runLoggerL :: LoggerRuntime -> L.LoggerL a -> IO a
runLoggerL lR = foldFree (interpretLoggerL lR)


instance L.Logger IO where
    logMessage text msg = runLoggerL LoggerRuntime $ L.logMessage text msg