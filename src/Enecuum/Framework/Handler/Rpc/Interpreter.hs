module Enecuum.Framework.Handler.Rpc.Interpreter where

import           Enecuum.Prelude

import qualified Data.Map as M
import           Enecuum.Framework.Handler.Rpc.Language

interpretRpcHandlerL :: TVar (M.Map Text (RpcHandler m)) -> RpcHandlerF m a -> IO a
interpretRpcHandlerL m (RpcHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runRpcHandlerL :: TVar (Map Text (RpcHandler m)) -> RpcHandlerL m a -> IO a
runRpcHandlerL m = foldFree (interpretRpcHandlerL m)

