{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Enecuum.Framework.Domain      as D
import           Enecuum.Framework.NetworkModel.Language       ( NetworkModel )

-- This is a raw view of mid-level networking. Will change significantly.
-- Supposed to be a mid-level language hiding WebSockets.

data NetworkingL a where
  OpenConnection :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
  CloseConnection :: D.Connection -> NetworkingL ()
  SendRequest :: D.Connection -> D.RpcRequest -> NetworkingL D.RpcResponse

  EvalNetwork :: Eff NetworkModel a -> NetworkingL a

makeFreer ''NetworkingL

withConnection
  :: Member NetworkingL effs
  => D.RpcMethod () req resp
  => D.ConnectionConfig
  -> req
  -> Eff effs (D.RpcResult resp)
withConnection cfg req = openConnection cfg >>= \case
  Nothing -> pure $ Left "Connecting failed."
  Just conn -> do
    rpcResponse <- sendRequest conn $ D.toRpcRequest () req
    case D.fromRpcResponse () rpcResponse of
      Nothing -> pure $ Left "Unknown RPC response."
      Just resp -> pure $ Right resp
