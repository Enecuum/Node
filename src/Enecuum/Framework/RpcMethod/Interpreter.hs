module Enecuum.Framework.RpcMethod.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free

import qualified Data.Map as M
import           Enecuum.Framework.RpcMethod.Language
import qualified Enecuum.Language                   as L
import           Enecuum.Core.Logger.Interpreter()

interpretRpcMethodL
    :: TVar (M.Map Text RpcMethod)
    -> RpcMethodL a
    -> IO a
interpretRpcMethodL m (RpcMethod name method next) = do
    L.logInfo $ "Addition of " +| name |+ " methode"
    atomically $ modifyTVar m (M.insert name method)
    pure (next ())

runRpcMethodL :: TVar (Map Text RpcMethod) -> Free RpcMethodL a -> IO a
runRpcMethodL m = foldFree ( interpretRpcMethodL m)