module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import qualified Network.WebSockets                 as WS
import qualified Data.Map                           as M
import           Data.Aeson                         as A

import    Control.Concurrent.STM.TChan
import    Enecuum.Legacy.Service.Network.Base

import Control.Monad.Free
import Enecuum.Framework.Node.Interpreter
import Enecuum.Framework.RpcMethod.Interpreter
import qualified Data.Map as M
import           Data.Aeson as A
import           Enecuum.Framework.RpcMethod.Language as L
import           Enecuum.Framework.Node.Language
import           Enecuum.Core.Logger.Interpreter
import           Enecuum.Legacy.Refact.Network.Server
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.Node.Runtime
import           Enecuum.Framework.Lens
import           Enecuum.Framework.NodeDefinition.Language 
import           Control.Lens.At

interpretNodeDefinitionL :: NodeRuntime -> NodeDefinitionF a -> IO a
interpretNodeDefinitionL _ (NodeTag tag next) = do
    pure $ next ()

interpretNodeDefinitionL nr (EvalNodeL initScript next) = do
    next <$> runNodeModel nr initScript

interpretNodeDefinitionL nr (ServingRpc port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    s <- atomically $ takeServerChan (nr^.servers) port
    void $ forkIO $ runRpcServer s port (runNodeModel nr) m
    return $ next a

interpretNodeDefinitionL nr (StopServing port next) = do
    atomically $ do
        serversMap <- readTVar (nr^.servers)
        whenJust (serversMap^.at port) $ \chan  -> writeTChan chan StopServer
    return $ next ()

-- TODO: treadDelay if server in port exist!!!
takeServerChan
    :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> STM (TChan ServerComand)
takeServerChan servs port = do
    serversMap <- readTVar servs
    whenJust (serversMap^.at port) $ \chan  -> writeTChan chan StopServer    
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


runNodeDefinitionL nr = foldFree (interpretNodeDefinitionL nr)
