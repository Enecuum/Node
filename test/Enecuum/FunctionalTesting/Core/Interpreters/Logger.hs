module Enecuum.FunctionalTesting.Core.Interpreters.Logger where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language           as L

import qualified Enecuum.FunctionalTesting.RLens as RLens
import qualified Enecuum.FunctionalTesting.Types as T


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
