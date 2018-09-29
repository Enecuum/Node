module Enecuum.Framework.RpcMethod.Interpreter where

import           Enecuum.Prelude

import qualified Data.Map as M
import           Enecuum.Framework.RpcMethod.Language

interpretRpcMethodL :: TVar (M.Map Text RpcMethod) -> RpcMethodF a -> IO a
interpretRpcMethodL m (RpcMethod name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runRpcMethodL :: TVar (Map Text RpcMethod) -> RpcMethodL a -> IO a
runRpcMethodL m = foldFree (interpretRpcMethodL m)
