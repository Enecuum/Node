{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Enecuum.Framework.Domain      as D
import           Enecuum.Framework.NetworkModel.Language       ( NetworkModel )

-- This is a raw view of mid-level networking. Will change significantly.
-- Supposed to be a mid-level language hiding WebSockets.

-- | Allows to work with network: open and close connections, send requests.
data NetworkingL a where
  -- | Open connection to the node.
  OpenConnection :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
  -- | Close existing connection.
  CloseConnection :: D.Connection -> NetworkingL ()

  -- TODO: we need more realistic model. Maybe, notion of sync / async requests.
  -- A real web sockets interpreter will show the truth.
  -- | Send RPC request with the connection.
  SendRequest :: D.Connection -> D.RpcRequest -> NetworkingL (D.RpcResult D.RpcResponse)

  -- | Eval low-level networking script.
  EvalNetwork :: Eff NetworkModel a -> NetworkingL a

makeFreer ''NetworkingL

-- TODO: this method should declare some error-proof.
-- It's probably wise to use `bracket` idiom here.

-- | Open connection, send request and close connection.
withConnection
  :: Member NetworkingL effs
  => D.RpcMethod () req resp
  => D.ConnectionConfig
  -> req
  -> Eff effs (D.RpcResult resp)
withConnection cfg req = openConnection cfg >>= \case
  Nothing -> pure $ Left "Connecting failed."
  Just conn -> do
    eRpcResponse <- sendRequest conn $ D.toRpcRequest () req
    closeConnection conn
    pure $ eRpcResponse >>= D.fromRpcResponse ()
