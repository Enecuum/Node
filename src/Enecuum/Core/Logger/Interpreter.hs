module Enecuum.Core.Logger.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free
import qualified Enecuum.Core.Language                      as L

-- | Interprets a LoggerL language.
-- Print a log msg in std out.
interpretLoggerL :: L.LoggerF a -> IO a
interpretLoggerL (L.LogMessage _ msg next) = do
    putStrLn msg
    pure $ next ()

-- | Runs the LoggerL language.
runLoggerL :: L.LoggerL a -> IO a
runLoggerL = foldFree interpretLoggerL


instance L.Logger IO where
    logMessage text msg = runLoggerL $ L.logMessage text msg