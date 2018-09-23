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
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.Runtime                 (NodeRuntime)
import qualified Enecuum.Framework.Lens
import qualified Enecuum.Framework.Language                as L
import qualified Enecuum.Core.RLens                        as RLens
import qualified Enecuum.Framework.RLens                   as RLens
import qualified Enecuum.Core.Interpreters                 as Impl
import qualified Enecuum.Framework.Node.Interpreter        as Impl


interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNodeL initScript next) =
    next <$> Impl.runNodeL nodeRt initScript

interpretNodeDefinitionL nodeRt (L.ServingRpc port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    s <- atomically $ takeServerChan (nodeRt ^. RLens.servers) port
    void $ forkIO $ runRpcServer s port (runNodeL nodeRt) m
    return $ next a

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    atomically $ do
        serversMap <- readTVar (nodeRt ^. RLens.servers)
        whenJust (serversMap ^. at port) $ \chan  -> writeTChan chan StopServer
    return $ next ()

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffect

-- TODO: treadDelay if server in port exist!!!
takeServerChan
    :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> STM (TChan ServerComand)
takeServerChan servs port = do
    serversMap <- readTVar servs
    whenJust (serversMap ^. at port) $ \chan  -> writeTChan chan StopServer
    chan <- newTChan
    modifyTVar servs (M.insert port chan)
    return chan

--runRpcServer :: TChan ServerComand -> PortNumber ->
runRpcServer chan port runner methodVar = do
    methods <- readTVarIO methodVar
    runServer chan port $ \_ pending -> do
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


runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
