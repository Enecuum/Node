module Enecuum.Core.Testing.Runtime.Logger.ImplMemory where

import           Enecuum.Prelude

import           Control.Monad.Free                      (foldFree)

import qualified Enecuum.Core.Language                   as L
import qualified Enecuum.Core.Logger.InterpreterHslogger as H (interpretLoggerL)
import           Enecuum.Core.Logger.Language
import qualified Enecuum.Core.Testing.Runtime.Lens       as RLens
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Core.Types.Logger               as T


-- | Interprets a LoggerL language.
-- Just pushes the messages into the concurrent list-like storage.
interpretLoggerL :: LoggerRuntimeMemory -> L.LoggerF a -> IO a
interpretLoggerL rt (L.LogMessage _ msg next) = do
  atomically $ modifyTVar (rt ^. RLens.messages) (msg :)
  pure $ next ()


-- | Runs the LoggerL language
-- save to memory
runLoggerL :: LoggerRuntimeMemory -> L.LoggerL a -> IO a
runLoggerL loggerRt = foldFree (interpretLoggerL loggerRt)
