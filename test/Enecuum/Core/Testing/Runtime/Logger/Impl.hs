module Enecuum.Core.Testing.Runtime.Logger.Impl where

import           Enecuum.Prelude

import           Control.Monad.Free                      (foldFree)

import qualified Enecuum.Core.Testing.Runtime.Lens       as RLens
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Core.Language                   as L
import qualified Enecuum.Core.Types                      as T


-- | Interprets a LoggerL language.
-- Just pushes the messages into the concurrent list-like storage.
interpretLoggerL :: LoggerRuntime -> L.LoggerF a -> IO a
interpretLoggerL loggerRt (L.LogMessage _ msg next) = do
  atomically $ modifyTVar (loggerRt ^. RLens.messages) (msg :)
  pure $ next ()

-- | Runs the LoggerL language
-- save to memory
runLoggerL :: LoggerRuntime -> L.LoggerL a -> IO a
runLoggerL loggerRt = foldFree (interpretLoggerL loggerRt)
