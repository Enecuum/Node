module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import qualified Network.WebSockets                 as WS
import           Control.Monad.Free
import qualified Data.Map as M
import           Data.Aeson as A

import qualified Enecuum.Language                   as L
import           Enecuum.Legacy.Service.Network.WebSockets.Server  (runServer)
import           Enecuum.Framework.RpcMethod.Language as L
import           Enecuum.Framework.Node.Language
import           Enecuum.Framework.Node.Language
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.Node.Runtime
import           Enecuum.Framework.Node.Interpreter      (runNodeL)
import           Enecuum.Framework.RpcMethod.Interpreter (runRpcMethodL)

interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF a -> IO a
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
    Just (RpcRequest method params reqId) -> case method `M.lookup` methods of
        Just justMethod -> runner $ justMethod params reqId
        Nothing -> return $ RpcResponseError
            (String $ "The method " <> method <> " is'nt supported.")
            reqId
    Nothing -> return $ RpcResponseError (String "error of request parsing") 0


runNodeDefinitionL nr = foldFree (interpretNodeDefinitionL nr)
