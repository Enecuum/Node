{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language         as L
import qualified Enecuum.Framework.Domain      as D
import           Enecuum.Framework.NetworkModel.Language       ( NetworkModel )

-- This is a raw view of mid-level networking. Will change significantly.
-- Supposed to be a mid-level language hiding WebSockets.

-- | Allows to work with network: open and close connections, send requests.
data NetworkingF next where
  -- | Open connection to the node.
  OpenConnection :: D.ConnectionConfig -> (Maybe D.Connection -> next) -> NetworkingF next
  -- | Close existing connection.
  CloseConnection :: D.Connection -> (() -> next) -> NetworkingF next

  -- TODO: we need more realistic model. Maybe, notion of sync / async requests.
  -- A real web sockets interpreter will show the truth.
  -- | Send RPC request with the connection.
  SendRequest :: D.Connection -> D.RpcRequest -> (D.RpcResult D.RpcResponse -> next) -> NetworkingF next

  -- | Eval low-level networking script.
  EvalNetwork :: NetworkModel a -> (a -> next) -> NetworkingF next

  -- | Eval core effect.
  EvalCoreEffectNetworkingF :: L.CoreEffectModel a -> (a -> next) -> NetworkingF next

instance Functor NetworkingF where
  fmap g (OpenConnection cfg next)          = OpenConnection cfg        (g . next)
  fmap g (CloseConnection conn next)        = CloseConnection conn      (g . next)
  fmap g (SendRequest conn rpcReq next)     = SendRequest conn rpcReq   (g . next)
  fmap g (EvalNetwork network next)         = EvalNetwork network       (g . next)
  fmap g (EvalCoreEffectNetworkingF coreEffect next) = EvalCoreEffectNetworkingF coreEffect (g . next)

type NetworkingL next = Free NetworkingF next

openConnection :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
openConnection cfg = liftF $ OpenConnection cfg id

closeConnection :: D.Connection -> NetworkingL ()
closeConnection conn = liftF $ CloseConnection conn id

sendRequest :: D.Connection -> D.RpcRequest -> NetworkingL (D.RpcResult D.RpcResponse)
sendRequest conn rpcReq = liftF $ SendRequest conn rpcReq id

evalNetwork :: NetworkModel a -> NetworkingL a 
evalNetwork network = liftF $ EvalNetwork network id

evalCoreEffectNetworkingF :: L.CoreEffectModel a -> NetworkingL a
evalCoreEffectNetworkingF coreEffect = liftF $ EvalCoreEffectNetworkingF coreEffect id

instance L.Logger (Free NetworkingF) where
  logMessage level msg = evalCoreEffectNetworkingF $ L.logMessage level msg


-- TODO: this method should declare some error-proof.
-- It's probably wise to use `bracket` idiom here.

-- | Open connection, send request and close connection.
withConnection
  :: D.RpcMethod () req resp
  => D.ConnectionConfig
  -> req
  -> NetworkingL (D.RpcResult resp)
withConnection cfg req = openConnection cfg >>= \case
  Nothing -> pure $ Left "Connecting failed."
  Just conn -> do
    eRpcResponse <- sendRequest conn $ D.toRpcRequest () req
    closeConnection conn
    pure $ eRpcResponse >>= D.fromRpcResponse ()
