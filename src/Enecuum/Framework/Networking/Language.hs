{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Enecuum.Core.Language         as L
import qualified Data.Text                     as Text
import qualified Enecuum.Framework.Domain      as D
import           Enecuum.Framework.NetworkModel.Language       ( NetworkModel )
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Legacy.Service.Network.Base (ConnectInfo(..))
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
  SendRequest :: D.Connection -> RpcRequest -> (Either Text RpcResponse -> next) -> NetworkingF next

  -- | Eval low-level networking script.
  EvalNetwork :: NetworkModel a -> (a -> next) -> NetworkingF next
  SendRpcRequest :: ConnectInfo -> RpcRequest -> (Either Text RpcResponse -> next) -> NetworkingF next

  -- | Eval core effect.
  EvalCoreEffectNetworkingF :: L.CoreEffectModel a -> (a -> next) -> NetworkingF next

instance Functor NetworkingF where
  fmap g (OpenConnection cfg next)          = OpenConnection cfg        (g . next)
  fmap g (CloseConnection conn next)        = CloseConnection conn      (g . next)
  fmap g (SendRequest conn rpcReq next)     = SendRequest conn rpcReq   (g . next)
  fmap g (EvalNetwork network next)         = EvalNetwork network       (g . next)
  fmap g (SendRpcRequest info request next) = SendRpcRequest info request (g . next)
  fmap g (EvalCoreEffectNetworkingF coreEffect next) = EvalCoreEffectNetworkingF coreEffect (g . next)

type NetworkingL next = Free NetworkingF next

openConnection :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
openConnection cfg = liftF $ OpenConnection cfg id

closeConnection :: D.Connection -> NetworkingL ()
closeConnection conn = liftF $ CloseConnection conn id

sendRequest :: D.Connection -> RpcRequest -> NetworkingL (Either Text  RpcResponse)
sendRequest conn rpcReq = liftF $ SendRequest conn rpcReq id

evalNetwork :: NetworkModel a -> NetworkingL a 
evalNetwork network = liftF $ EvalNetwork network id

sendRpcRequest :: ConnectInfo -> RpcRequest -> NetworkingL (Either Text RpcResponse)
sendRpcRequest info request = liftF $ SendRpcRequest info request id

evalCoreEffectNetworkingF :: L.CoreEffectModel a -> NetworkingL a
evalCoreEffectNetworkingF coreEffect = liftF $ EvalCoreEffectNetworkingF coreEffect id

instance L.Logger (Free NetworkingF) where
  logMessage level msg = evalCoreEffectNetworkingF $ L.logMessage level msg


-- TODO: this method should declare some error-proof.
-- It's probably wise to use `bracket` idiom here.

-- | Open connection, send request and close connection.
withConnection :: D.ConnectionConfig -> RpcRequest -> NetworkingL (Either Text RpcResponse)
withConnection cfg req = openConnection cfg >>= \case
  Nothing -> pure $ Left "Connecting failed."
  Just conn -> do
    response <- sendRequest conn req
    closeConnection conn
    pure response


--
makeRpcRequest
    :: (ToJSON a, FromJSON b) => D.ConnectionConfig -> Text -> a -> NetworkingL (Either Text b)
makeRpcRequest connectCfg name arg = do
    res <- withConnection connectCfg (makeRequest name arg)
    case res of
        Left txt -> pure $ Left txt
        Right (RpcResponseError (A.String txt) _) -> pure $ Left txt
        Right (RpcResponseError err _)            -> pure $ Left (show err)
        Right (RpcResponseResult val _) -> case A.fromJSON val of
            A.Error txt -> pure $ Left (Text.pack txt)
            A.Success resp -> pure $ Right resp
            