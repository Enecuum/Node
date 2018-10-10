module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude


import qualified Data.Map                           as M
import           Data.Aeson                         as A
import           Control.Concurrent.STM.TChan
import qualified Network.Socket.ByteString.Lazy     as S
import qualified Network.Socket                     as S hiding (recv)
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Framework.Networking.Internal.TCP.Server

import           Enecuum.Framework.Node.Interpreter
import           Enecuum.Framework.RpcMethod.Interpreter
import           Enecuum.Framework.Runtime                 (NodeRuntime)

import qualified Enecuum.Framework.Language                as L
import qualified Enecuum.Framework.RLens                   as RLens
import qualified Enecuum.Core.Interpreters                 as Impl
import qualified Enecuum.Framework.Node.Interpreter        as Impl
import qualified Enecuum.Framework.Domain.RPC             as D
import qualified Enecuum.Framework.Domain.Networking      as D
import           Enecuum.Framework.Networking.Internal.Internal
import           Enecuum.Framework.MsgHandler.Interpreter
import           Enecuum.Framework.StdinHandlers.Interpreter as Imp
import           Data.Aeson.Lens
import qualified Data.Text as T

interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNodeL initScript next) = next <$> Impl.runNodeL nodeRt initScript

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffect

interpretNodeDefinitionL nodeRt (L.ServingMsg port initScript next) = do
    m        <- atomically $ newTVar mempty
    a        <- runMsgHandlerL m initScript
    handlers <- readTVarIO m
    s        <- startServer port
                            ((\f a b -> Impl.runNodeL nodeRt $ f a b) <$> handlers)
                            (\(D.NetworkConnection addr) -> Impl.insertConnect (nodeRt ^. RLens.connects) addr)
    atomically $ setServerChan (nodeRt ^. RLens.servers) port s
    pure $ next a

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    atomically $ do
        serversMap <- readTVar (nodeRt ^. RLens.servers)
        whenJust (serversMap ^. at port) stopServer
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.ServingRpc port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    s <- atomically $ takeServerChan (nodeRt ^. RLens.servers) port
    void $ forkIO $ runRpcServer s port (runNodeL nodeRt) m
    pure $ next a

interpretNodeDefinitionL nodeRt (L.Std handlers next) = do
    m <- atomically $ newTVar mempty
    a <- runStdinHandlerL m handlers
    void $ forkIO $ do
        m' <- readTVarIO m
        forever $ do
            line <- getLine
            res  <- callHandler nodeRt m' line
            putTextLn res
    pure $ next ()

--
callHandler :: NodeRuntime -> Map Text (Value -> L.NodeL Text) -> Text -> IO Text
callHandler nodeRt methods msg = do
    let val = A.decode $ fromString $ T.unpack msg
    case val of
        Just ((^? key "method" . _String ) -> Just method) -> case methods ^. at method of
            Just justMethod -> Impl.runNodeL nodeRt $ justMethod (fromJust $ val)
            Nothing         -> pure $ "The method " <> method <> " isn't supported."
        Nothing -> pure "Error of request parsing."


fromJust (Just a) = a
--
-- TODO: treadDelay if server in port exist!!!
takeServerChan :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> STM (TChan ServerComand)
takeServerChan servs port = do
    chan <- newTChan
    Impl.setServerChan servs port chan
    pure chan


runRpcServer
    :: TChan ServerComand -> PortNumber -> (t -> IO D.RpcResponse) -> TVar (Map Text (A.Value -> Int -> t)) -> IO ()
runRpcServer chan port runner methodVar = do
    methods <- readTVarIO methodVar
    runServer chan port $ \sock -> do
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
