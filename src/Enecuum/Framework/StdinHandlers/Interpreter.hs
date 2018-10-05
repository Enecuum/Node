module Enecuum.Framework.StdinHandlers.Interpreter where

import           Enecuum.Prelude

import qualified Data.Map as M
import           Enecuum.Framework.StdinHandlers.Language

interpretStdinHandlerL :: TVar (M.Map Text StdinHandler) -> StdinHandlersF a -> IO a
interpretStdinHandlerL m (StdinHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runStdinHandlerL :: TVar (Map Text StdinHandler) -> StdinHandlerL a -> IO a
runStdinHandlerL m = foldFree (interpretStdinHandlerL m)

    