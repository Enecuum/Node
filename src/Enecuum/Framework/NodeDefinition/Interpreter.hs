module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import qualified Network.WebSockets                 as WS
import qualified Data.Map                           as M
import           Data.Aeson                         as A
import           Control.Concurrent.STM.TChan

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Refact.Network.Server

import           Enecuum.Framework.Node.Interpreter
import           Enecuum.Framework.RpcMethod.Interpreter
import           Enecuum.Framework.Domain.RPC
import           Enecuum.Framework.Runtime                 (NodeRuntime)

import qualified Enecuum.Framework.Language                as L
import qualified Enecuum.Framework.RLens                   as RLens
import qualified Enecuum.Core.Interpreters                 as Impl
import qualified Enecuum.Framework.Node.Interpreter        as Impl
import qualified Enecuum.Framework.Domain.RPC             as D
import qualified Enecuum.Framework.Domain.Networking      as D
import qualified Data.Map                                 as M
import qualified Data.Aeson                               as A
import qualified Network.WebSockets                       as WS
import           Control.Concurrent.STM.TChan
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Refact.Network.Server
import           Enecuum.Framework.RpcMethod.Interpreter
import           Enecuum.Framework.Networking.Internal 
import           Enecuum.Framework.MsgHandler.Interpreter


interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF  a -> IO a
interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNodeL initScript next) =
    next <$> Impl.runNodeL nodeRt initScript

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffect

interpretNodeDefinitionL nodeRt (L.ServingMsg port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runMsgHandlerL m initScript
    handlers <- readTVarIO m
    s <- startServer port 
        ((\f a b -> Impl.runNodeL nodeRt $ f a b) <$> handlers)
        (\(D.NetworkConnection addr) -> Impl.insertConnect (nodeRt ^. RLens.connects) addr)
    atomically $ setServerChan (nodeRt ^. RLens.servers) port s
    return $ next a

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    atomically $ do
        serversMap <- readTVar (nodeRt ^. RLens.servers)
        whenJust (serversMap ^. at port) stopServer
    return $ next ()

--
interpretNodeDefinitionL nodeRt (L.ServingRpc port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    s <- atomically $ takeServerChan (nodeRt ^. RLens.servers) port
    void $ forkIO $ runRpcServer s port (runNodeL nodeRt) m
    return $ next a


--
-- TODO: treadDelay if server in port exist!!!
takeServerChan
    :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> STM (TChan ServerComand)
takeServerChan servs port = do
    chan <- newTChan
    Impl.setServerChan servs port chan
    return chan


runRpcServer
    :: TChan ServerComand
    -> PortNumber
    -> (t -> IO D.RpcResponse)
    -> TVar (Map Text (A.Value -> Int -> t))
    -> IO ()
runRpcServer chan port runner methodVar = do
    methods <- readTVarIO methodVar
    runServer chan port $ \_ pending -> do
        connect     <- WS.acceptRequest pending
        msg         <- WS.receiveData connect
        response    <- callRpc runner methods msg
        WS.sendTextData connect $ A.encode response

callRpc :: Monad m => (t -> m D.RpcResponse) -> Map Text (A.Value -> Int -> t) -> ByteString -> m D.RpcResponse
callRpc runner methods msg = case A.decodeStrict msg of
    Just (D.RpcRequest method params reqId) -> case method `M.lookup` methods of
        Just justMethod -> runner $ justMethod params reqId
        Nothing -> return $ D.RpcResponseError
            (A.String $ "The method " <> method <> " is'nt supported.")
            reqId
    Nothing -> return $ D.RpcResponseError (A.String "error of request parsing") 0


runNodeDefinitionL :: NodeRuntime -> Free L.NodeDefinitionF a -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
