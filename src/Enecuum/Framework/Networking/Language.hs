{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                           as A
import qualified Enecuum.Core.Language                as L
import qualified Data.Text                            as Text
import qualified Enecuum.Framework.Domain             as D
import           Enecuum.Framework.Network.Language   (NetworkL)
import           Enecuum.Framework.Domain.RpcMessages

-- | Allows to work with network: open and close connections, send requests.
data NetworkingF next where
  -- | Open connection to the node.
  OpenConnection :: D.Address -> (Maybe D.NetworkConnection -> next) -> NetworkingF next
  -- | Close existing connection.
  CloseConnection :: D.NetworkConnection -> (() -> next) -> NetworkingF  next

  -- | Send message to the the connection.
  SendRequest :: D.NetworkConnection -> RpcRequest -> (Either Text RpcResponse -> next) -> NetworkingF next

  -- | Eval low-level networking script.
  EvalNetwork :: NetworkL a -> (a -> next) -> NetworkingF  next
  SendRpcRequest :: D.Address -> RpcRequest -> (Either Text RpcResponse -> next) -> NetworkingF next

  -- | Eval core effect.
  EvalCoreEffectNetworkingF :: L.CoreEffect a -> (a -> next) -> NetworkingF  next

instance Functor NetworkingF where
  fmap g (OpenConnection address next)      = OpenConnection address    (g . next)
  fmap g (CloseConnection conn next)        = CloseConnection conn      (g . next)
  fmap g (SendRequest conn rpcReq next)     = SendRequest conn rpcReq   (g . next)
  fmap g (EvalNetwork network next)         = EvalNetwork network       (g . next)
  fmap g (SendRpcRequest info request next) = SendRpcRequest info request (g . next)
  fmap g (EvalCoreEffectNetworkingF coreEffect next) = EvalCoreEffectNetworkingF coreEffect (g . next)

type NetworkingL  next = Free NetworkingF next

openConnection :: D.Address -> NetworkingL (Maybe D.NetworkConnection)
openConnection address = liftF $ OpenConnection address id

closeConnection :: D.NetworkConnection -> NetworkingL  ()
closeConnection conn = liftF $ CloseConnection conn id

sendRequest :: D.NetworkConnection -> RpcRequest -> NetworkingL (Either Text RpcResponse)
sendRequest conn rpcReq = liftF $ SendRequest conn rpcReq id

evalNetwork :: NetworkL a -> NetworkingL  a
evalNetwork network = liftF $ EvalNetwork network id

sendRpcRequest :: D.Address -> RpcRequest -> NetworkingL (Either Text RpcResponse)
sendRpcRequest address request = liftF $ SendRpcRequest address request id

evalCoreEffectNetworkingF :: L.CoreEffect a -> NetworkingL a
evalCoreEffectNetworkingF coreEffect = liftF $ EvalCoreEffectNetworkingF coreEffect id

instance L.Logger (Free NetworkingF) where
  logMessage level msg = evalCoreEffectNetworkingF $ L.logMessage level msg

makeRpcRequest'
    :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> NetworkingL (Either Text b)
makeRpcRequest' address arg =
    responseValidation =<< sendRpcRequest address (makeRequest arg)
--
--
-- makeRpcRequest_
--     :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> NetworkingL  (Either Text b)
-- makeRpcRequest_ connectCfg arg =
--     responseValidation =<< withConnection connectCfg (makeRequest arg)


responseValidation :: (FromJSON b, Applicative f) => Either Text RpcResponse -> f (Either Text b)
responseValidation res = case res of
  Left txt -> pure $ Left txt
  Right (RpcResponseError (A.String txt) _) -> pure $ Left txt
  Right (RpcResponseError err _)            -> pure $ Left (show err)
  Right (RpcResponseResult val _) -> case A.fromJSON val of
      A.Error txt -> pure $ Left (Text.pack txt)
      A.Success resp -> pure $ Right resp
