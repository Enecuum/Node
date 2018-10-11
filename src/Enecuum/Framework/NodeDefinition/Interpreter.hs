module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude


import qualified Data.Map                           as M
import           Data.Aeson                         as A
import           Control.Concurrent.STM.TChan
import qualified Network.Socket.ByteString.Lazy     as S
import qualified Network.Socket                     as S hiding (recv)
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Framework.Networking.Internal.Tcp.Server

import           Enecuum.Framework.Node.Interpreter (runNodeL, setServerChan)
import           Enecuum.Framework.Runtime                 (NodeRuntime, getNextId)

import qualified Enecuum.Framework.Language                as L
import qualified Enecuum.Framework.RLens                   as RLens
import qualified Enecuum.Core.Interpreters                 as Impl
import qualified Enecuum.Framework.Node.Interpreter        as Impl
import qualified Enecuum.Framework.Domain.RPC              as D
import qualified Enecuum.Framework.Domain.Networking       as D
import qualified Enecuum.Framework.Domain.Process          as D
import           Enecuum.Framework.Networking.Internal.Tcp.Connection
import           Enecuum.Framework.Handler.Rpc.Interpreter
import           Enecuum.Framework.Handler.Tcp.Interpreter
import           Enecuum.Framework.Handler.Cmd.Interpreter as Imp
import           Data.Aeson.Lens
import qualified Data.Text as T

addProcess :: NodeRuntime -> D.ProcessPtr a -> ThreadId -> IO ()
addProcess nodeRt pPtr threadId = do
    pId <- D.getProcessId pPtr
    ps <- atomically $ takeTMVar $ nodeRt ^. RLens.processes
    let newPs = M.insert pId threadId ps
    atomically $ putTMVar (nodeRt ^. RLens.processes) newPs

interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNodeL action next) = next <$> Impl.runNodeL nodeRt action

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffect

interpretNodeDefinitionL nodeRt (L.ServingTcp port action next) = do
    m        <- atomically $ newTVar mempty
    a        <- runTcpHandlerL m action
    handlers <- readTVarIO m
    s        <- startServer
        port
        ((\f a b -> Impl.runNodeL nodeRt $ f a b) <$> handlers)
        (\(D.TcpConnection addr) -> Impl.insertConnect (nodeRt ^. RLens.connects) addr)
    atomically $ setServerChan (nodeRt ^. RLens.servers) port s
    pure $ next a

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    atomically $ do
        serversMap <- readTVar (nodeRt ^. RLens.servers)
        whenJust (serversMap ^. at port) stopServer
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.ServingRpc port action next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcHandlerL m action
    s <- atomically $ takeServerChan (nodeRt ^. RLens.servers) port
    void $ forkIO $ runRpcServer s port (runNodeL nodeRt) m
    pure $ next a

interpretNodeDefinitionL nodeRt (L.Std handlers next) = do
    m <- atomically $ newTVar mempty
    a <- runCmdHandlerL m handlers
    void $ forkIO $ do
        m' <- readTVarIO m
        forever $ do
            line <- getLine
            res  <- callHandler nodeRt m' line
            putTextLn res
    pure $ next ()

-- TODO: make a separate language and use its interpreter in test runtime too.
interpretNodeDefinitionL nodeRt (L.ForkProcess action next) = do
    (pPtr, pVar) <- atomically (getNextId nodeRt) >>= D.createProcessPtr
    threadId <- forkIO $ do
        res <- runNodeL nodeRt action
        atomically $ putTMVar pVar res
    addProcess nodeRt pPtr threadId
    pure $ next pPtr

interpretNodeDefinitionL _ (L.TryGetResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    mbResult <- atomically $ tryReadTMVar pVar
    pure $ next mbResult

interpretNodeDefinitionL _ (L.AwaitResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    result <- atomically $ takeTMVar pVar
    pure $ next result

--
callHandler :: NodeRuntime -> Map Text (Value -> L.NodeL Text) -> Text -> IO Text
callHandler nodeRt methods msg = do
    let val = A.decode $ fromString $ T.unpack msg
    case val of
        Just ((^? key "method" . _String) -> Just method) -> case methods ^. at method of
            Just justMethod -> Impl.runNodeL nodeRt $ justMethod (fromJust $ val)
            Nothing         -> pure $ "The method " <> method <> " isn't supported."
        Nothing -> pure "Error of request parsing."


fromJust (Just a) = a
--
-- TODO: treadDelay if server in port exist!!!
takeServerChan :: TVar (Map PortNumber (TChan D.ServerComand)) -> PortNumber -> STM (TChan D.ServerComand)
takeServerChan servs port = do
    chan <- newTChan
    Impl.setServerChan servs port chan
    pure chan


runRpcServer
    :: TChan D.ServerComand -> PortNumber -> (t -> IO D.RpcResponse) -> TVar (Map Text (A.Value -> Int -> t)) -> IO ()
runRpcServer chan port runner methodVar = do
    methods <- readTVarIO methodVar
    runTCPServer chan port $ \sock -> do
        msg      <- S.recv sock (1024 * 4)
        response <- callRpc runner methods msg
        S.sendAll sock $ A.encode response

callRpc :: Monad m => (t -> m D.RpcResponse) -> Map Text (A.Value -> Int -> t) -> LByteString -> m D.RpcResponse
callRpc runner methods msg = case A.decode msg of
    Just (D.RpcRequest method params reqId) -> case method `M.lookup` methods of
        Just justMethod -> runner $ justMethod params reqId
        Nothing         -> pure $ D.RpcResponseError (A.String $ "The method " <> method <> " isn't supported.") reqId
    Nothing -> pure $ D.RpcResponseError (A.String "error of request parsing") 0


runNodeDefinitionL :: NodeRuntime -> Free L.NodeDefinitionF a -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
