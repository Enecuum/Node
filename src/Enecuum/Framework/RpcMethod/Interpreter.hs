module Enecuum.Framework.RpcMethod.Interpreter where

import           Enecuum.Prelude

import qualified Data.Map as M
import           Enecuum.Framework.RpcMethod.Language

interpretRpcMethodL :: TVar (M.Map Text (RpcMethod m)) -> RpcMethodF m a -> IO a
interpretRpcMethodL m (RpcMethod name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runRpcMethodL :: TVar (Map Text (RpcMethod m)) -> RpcMethodL m a -> IO a
runRpcMethodL m = foldFree (interpretRpcMethodL m)

