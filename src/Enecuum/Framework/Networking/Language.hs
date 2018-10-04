{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                           as A
import qualified Enecuum.Core.Language                as L
import qualified Enecuum.Framework.Network.Language   as L
import qualified Data.Text                            as Text
import qualified Enecuum.Framework.Domain             as D
import qualified Enecuum.Framework.MsgHandler.Language as L

-- | Allows to work with network: open and close connections, send requests.
data NetworkingF next where
  -- | Eval low-level networking script.
  EvalNetwork :: L.NetworkL a -> (a -> next) -> NetworkingF  next
  -- | Send RPC request and wait for the response.
  SendRpcRequest :: D.Address -> D.RpcRequest -> (Either Text D.RpcResponse -> next) -> NetworkingF next
  -- | Send message to the connection.
  SendMessage :: D.NetworkConnection -> LByteString -> (() -> next)-> NetworkingF next
  -- | Eval core effect.
  EvalCoreEffectNetworkingF :: L.CoreEffect a -> (a -> next) -> NetworkingF  next

instance Functor NetworkingF where
  fmap g (EvalNetwork network next)                  = EvalNetwork network                  (g . next)
  fmap g (SendRpcRequest info request next)          = SendRpcRequest info request          (g . next)
  fmap g (SendMessage conn msg next)                 = SendMessage conn msg                 (g . next)
  fmap g (EvalCoreEffectNetworkingF coreEffect next) = EvalCoreEffectNetworkingF coreEffect (g . next)

type NetworkingL  next = Free NetworkingF next

-- | Eval low-level networking script.
evalNetwork :: L.NetworkL a -> NetworkingL  a
evalNetwork network = liftF $ EvalNetwork network id

-- | Send RPC request and wait for the response.
sendRpcRequest :: D.Address -> D.RpcRequest -> NetworkingL (Either Text D.RpcResponse)
sendRpcRequest address request = liftF $ SendRpcRequest address request id

-- | Send message to the connection.
sendMessage :: D.NetworkConnection -> LByteString -> NetworkingL ()
sendMessage conn msg = liftF $ SendMessage conn msg id

-- | Send message to the reliable connection.
-- TODO: distiguish reliable (TCP-like) connection from unreliable (UDP-like).
-- TODO: make conversion to and from package.
class Send m where
  send :: (Typeable a, ToJSON a) => D.NetworkConnection -> a -> m ()

instance Send (Free NetworkingF) where
  send conn msg = sendMessage conn . A.encode $
    D.NetworkMsg (L.makeTagName msg) (toJSON msg)

-- | Eval core effect.
evalCoreEffectNetworkingF :: L.CoreEffect a -> NetworkingL a
evalCoreEffectNetworkingF coreEffect = liftF $ EvalCoreEffectNetworkingF coreEffect id



instance L.Logger (Free NetworkingF) where
  logMessage level msg = evalCoreEffectNetworkingF $ L.logMessage level msg

makeRpcRequest'
    :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> NetworkingL  (Either Text b)
makeRpcRequest' address arg =
    responseValidation =<< sendRpcRequest address (D.toRpcRequest arg)

instance L.ERandom (Free NetworkingF) where
    getRandomInt = evalCoreEffectNetworkingF . L.getRandomInt
    evalRand r g = evalCoreEffectNetworkingF $ L.evalRand r g

responseValidation :: (FromJSON b, Applicative f) => Either Text D.RpcResponse -> f (Either Text b)
responseValidation res = case res of
  Left txt -> pure $ Left txt
  Right (D.RpcResponseError (A.String txt) _) -> pure $ Left txt
  Right (D.RpcResponseError err _)            -> pure $ Left (show err)
  Right (D.RpcResponseResult val _) -> case A.fromJSON val of
      A.Error txt    -> pure $ Left (Text.pack txt)
      A.Success resp -> pure $ Right resp
