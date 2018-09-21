module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import qualified Network.WebSockets                 as WS
import qualified Data.Map                           as M
import           Data.Aeson                         as A

import           Enecuum.Legacy.Service.Network.WebSockets.Server  (runServer)

import qualified Enecuum.Language                        as L
import qualified Enecuum.Framework.Runtime               as Rt
import qualified Enecuum.Framework.Domain                as D
import           Enecuum.Framework.Node.Interpreter      (runNodeL)
import           Enecuum.Framework.RpcMethod.Interpreter (runRpcMethodL)

interpretNodeDefinitionL :: Rt.NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL _ (L.NodeTag tag next) = do
    pure $ next ()

interpretNodeDefinitionL nr (L.EvalNodeL initScript next) = do
    next <$> runNodeL nr initScript

interpretNodeDefinitionL nr (L.ServingRpc port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    void $ forkIO $ runRpcServer port (runNodeL nr) m
    return $ next a

runRpcServer port runner methodVar = do
    methods <- readTVarIO methodVar
    runServer port "/" $ \_ pending -> do
        connect     <- WS.acceptRequest pending
        msg         <- WS.receiveData connect
        response    <- callRpc runner methods msg
        WS.sendTextData connect $ A.encode response


callRpc runner methods msg = case A.decodeStrict msg of
    Just (D.RpcRequest method params reqId) -> case method `M.lookup` methods of
        Just justMethod -> runner $ justMethod params reqId
        Nothing -> return $ D.RpcResponseError
            (String $ "The method " <> method <> " is'nt supported.")
            reqId
    Nothing -> return $ D.RpcResponseError (String "error of request parsing") 0


runNodeDefinitionL nr = foldFree (interpretNodeDefinitionL nr)
