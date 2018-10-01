module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Framework.Networking.Interpreter as Impl
import           Enecuum.Framework.Runtime                (NodeRuntime)
import qualified Enecuum.Core.Interpreters                as Impl
import qualified Enecuum.Framework.State.Interpreter      as Impl
import qualified Enecuum.Framework.RLens                  as RLens
import qualified Enecuum.Framework.Domain.RPC             as D
import qualified Data.Map                                 as M
import qualified Data.Aeson                               as A
import qualified Network.WebSockets                       as WS
import           Control.Concurrent.STM.TChan
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Refact.Network.Server
import           Enecuum.Framework.RpcMethod.Interpreter

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL _ (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) =
  next <$> ioRunner act

interpretNodeL _ (L.EvalNetworking networking next) =
    next <$> Impl.runNetworkingL networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.StopNode next) = do
    atomically $ putTMVar (nodeRt ^. RLens.stopNode) True
    return $ next ()

interpretNodeL nodeRt (L.ServingRpc port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    s <- atomically $ takeServerChan (nodeRt ^. RLens.servers) port
    void $ forkIO $ runRpcServer s port (runNodeL nodeRt) m
    return $ next a

interpretNodeL nodeRt (L.StopServing port next) = do
    atomically $ do
        serversMap <- readTVar (nodeRt ^. RLens.servers)
        whenJust (serversMap ^. at port) $ \chan  -> writeTChan chan StopServer
    return $ next ()

--
-- TODO: treadDelay if server in port exist!!!
takeServerChan
    :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> STM (TChan ServerComand)
takeServerChan servs port = do
    serversMap <- readTVar servs
    whenJust (serversMap ^. at port) $ \chan  -> writeTChan chan StopServer
    chan <- newTChan
    modifyTVar servs (M.insert port chan)
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

{-



  fmap g (EvalStateAtomically statefulAction next) = EvalStateAtomically statefulAction (g . next)
  fmap g (EvalNetworking networking next)          = EvalNetworking networking          (g . next)
  fmap g (EvalCoreEffectNodeF coreEffect next)     = EvalCoreEffectNodeF coreEffect     (g . next)
  fmap g (EvalGraphIO graphAction next)            = EvalGraphIO graphAction            (g . next)
  fmap g (StopNode next)                           = StopNode                           (g . next)
  fmap g (ServingRpc port handlersF next)          = ServingRpc port handlersF          (g . next)
  fmap g (StopServing port next)                   = StopServing port                   (g . next)
  fmap g (OpenConnection a b next)                 = OpenConnection  a b                (g . next)
  fmap g (ServingMsg a b next)                     = ServingMsg a b                     (g . next)
  fmap g (CloseConnection a next)                  = CloseConnection a                  (g . next)

-}

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
