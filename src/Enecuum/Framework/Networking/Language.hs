{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                           as A
import qualified Enecuum.Core.Language                as L
import qualified Enecuum.Framework.Network.Language   as L
import qualified Data.Text                            as Text
import qualified Enecuum.Framework.Domain             as D

-- | Allows to work with network: open and close connections, send requests.
data NetworkingF next where
  -- | Eval low-level networking script.
  EvalNetwork :: L.NetworkL a -> (a -> next) -> NetworkingF  next
  SendRpcRequest :: D.Address -> D.RpcRequest -> (Either Text D.RpcResponse -> next) -> NetworkingF next

  -- | Eval core effect.
  EvalCoreEffectNetworkingF :: L.CoreEffect a -> (a -> next) -> NetworkingF  next

instance Functor NetworkingF where
  fmap g (EvalNetwork network next)         = EvalNetwork network       (g . next)
  fmap g (SendRpcRequest info request next) = SendRpcRequest info request (g . next)
  fmap g (EvalCoreEffectNetworkingF coreEffect next) = EvalCoreEffectNetworkingF coreEffect (g . next)

type NetworkingL  next = Free NetworkingF next

evalNetwork :: L.NetworkL a -> NetworkingL  a
evalNetwork network = liftF $ EvalNetwork network id

sendRpcRequest :: D.Address -> D.RpcRequest -> NetworkingL (Either Text D.RpcResponse)
sendRpcRequest address request = liftF $ SendRpcRequest address request id

evalCoreEffectNetworkingF :: L.CoreEffect a -> NetworkingL a
evalCoreEffectNetworkingF coreEffect = liftF $ EvalCoreEffectNetworkingF coreEffect id

instance L.Logger (Free NetworkingF) where
  logMessage level msg = evalCoreEffectNetworkingF $ L.logMessage level msg

makeRpcRequest'
    :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> NetworkingL  (Either Text b)
makeRpcRequest' address arg =
    responseValidation =<< sendRpcRequest address (D.toRpcRequest arg)


responseValidation :: (FromJSON b, Applicative f) => Either Text D.RpcResponse -> f (Either Text b)
responseValidation res = case res of
  Left txt -> pure $ Left txt
  Right (D.RpcResponseError (A.String txt) _) -> pure $ Left txt
  Right (D.RpcResponseError err _)            -> pure $ Left (show err)
  Right (D.RpcResponseResult val _) -> case A.fromJSON val of
      A.Error txt    -> pure $ Left (Text.pack txt)
      A.Success resp -> pure $ Right resp
