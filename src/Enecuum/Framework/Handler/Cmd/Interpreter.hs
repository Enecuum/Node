module Enecuum.Framework.Handler.Cmd.Interpreter where

import           Enecuum.Prelude

import qualified Data.Map as M
import           Enecuum.Framework.Handler.Cmd.Language

interpretCmdHandlerL :: TVar (M.Map Text CmdHandler) -> CmdHandlerF a -> IO a
interpretCmdHandlerL m (CmdHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runCmdHandlerL :: TVar (Map Text CmdHandler) -> CmdHandlerL a -> IO a
runCmdHandlerL m = foldFree (interpretCmdHandlerL m)
