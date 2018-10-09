module Enecuum.Testing.Core.Interpreters.Logger where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language as L

import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Types as T


-- | Interprets a LoggerL language.
-- Just pushes the messages into the concurrent list-like storage.
interpretLoggerL :: T.LoggerRuntime -> L.LoggerF a -> IO a
interpretLoggerL loggerRt (L.LogMessage _ msg next) = do
    atomically $ modifyTVar (loggerRt ^. RLens.messages) (msg :)
    pure $ next ()

-- | Runs the LoggerL language
-- save to memory
runLoggerL :: T.LoggerRuntime -> L.LoggerL a -> IO a
runLoggerL loggerRt = foldFree (interpretLoggerL loggerRt)
