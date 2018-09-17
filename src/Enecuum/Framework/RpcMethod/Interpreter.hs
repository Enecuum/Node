module Enecuum.Framework.RpcMethod.Interpreter where

import           Enecuum.Prelude
import           Eff                                ( handleRelay )
import           Enecuum.Framework.Node.Language
import           Data.Aeson as A
import qualified Data.Map as M
import           Enecuum.Framework.RpcMethod.Language
import qualified Enecuum.Language                   as L

interpretRpcMethodL
    :: TVar (M.Map Text RpcMethod)
    -> RpcMethodL a
    -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretRpcMethodL m (RpcMethod name method) = do
    L.logInfo $ "Addition of " +| name |+ " methode"
    safeIO $ atomically $ modifyTVar m (M.insert name method)


    
--runRpcMethodL :: TVar (M.Map Text (A.Value -> Eff NodeModel A.Value)) 
runRpcMethodL m = handleRelay pure ( (>>=) . interpretRpcMethodL m)