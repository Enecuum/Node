{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.Networking.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                           as A
import qualified Enecuum.Core.Language                as L
import qualified Enecuum.Framework.Network.Language   as L
import qualified Data.Text                            as Text
import qualified Enecuum.Framework.Domain             as D
import           Language.Haskell.TH.MakeFunctor

-- | Allows to work with network: open and close connections, send requests.
data NetworkingF next where
  -- | Eval low-level networking script.
  EvalNetwork               :: L.NetworkL a -> (a -> next) -> NetworkingF  next
  -- | Send RPC request and wait for the response.
  SendRpcRequest            :: D.Address -> D.RpcRequest -> (Either Text D.RpcResponse -> next) -> NetworkingF next
  -- | Send message to the connection.
  SendTcpMsgByConnection    :: D.Connection D.Tcp -> D.RawData -> (Either D.NetworkError () -> next)-> NetworkingF next
  SendUdpMsgByConnection    :: D.Connection D.Udp -> D.RawData -> (Either D.NetworkError () -> next)-> NetworkingF next
  SendUdpMsgByAddress       :: D.Address          -> D.RawData -> (Either D.NetworkError () -> next)-> NetworkingF next
  -- | Eval core effect.
  EvalCoreEffectNetworkingF :: L.CoreEffect a -> (a -> next) -> NetworkingF  next

makeFunctorInstance ''NetworkingF

type NetworkingL next = Free NetworkingF next

-- | Eval low-level networking script.
evalNetwork :: L.NetworkL a -> NetworkingL a
evalNetwork network = liftF $ EvalNetwork network id

-- | Send RPC request and wait for the response.
sendRpcRequest :: D.Address -> D.RpcRequest -> NetworkingL (Either Text D.RpcResponse)
sendRpcRequest address request = liftF $ SendRpcRequest address request id

-- | Send message to the reliable connection.
-- TODO: distiguish reliable (TCP-like) connection from unreliable (UDP-like).
-- TODO: make conversion to and from package.

toNetworkMsg :: (Typeable a, ToJSON a) => a -> D.RawData
toNetworkMsg msg = A.encode $ D.NetworkMsg (D.toTag msg) (toJSON msg)

-- | Send message to the connection.
class Send con m where
    send :: (Typeable a, ToJSON a) => con -> a -> m (Either D.NetworkError ())

instance Send (D.Connection D.Tcp) (Free NetworkingF) where
    send conn msg = liftF $ SendTcpMsgByConnection conn (toNetworkMsg msg) id

instance Send (D.Connection D.Udp) (Free NetworkingF) where
    send conn msg = liftF $ SendUdpMsgByConnection conn (toNetworkMsg msg) id
instance SendUdp (Free NetworkingF) where
    notify conn msg = liftF $ SendUdpMsgByAddress conn (toNetworkMsg msg) id

class SendUdp m where
    notify :: (Typeable a, ToJSON a) => D.Address -> a -> m (Either D.NetworkError ())

-- | Eval core effect.
evalCoreEffectNetworkingF :: L.CoreEffect a -> NetworkingL a
evalCoreEffectNetworkingF coreEffect = liftF $ EvalCoreEffectNetworkingF coreEffect id

instance L.Logger (Free NetworkingF) where
  logMessage level msg = evalCoreEffectNetworkingF $ L.logMessage level msg

makeRpcRequest' :: (Typeable a, ToJSON a, FromJSON b) => D.Address -> a -> NetworkingL (Either Text b)
makeRpcRequest' address arg = responseValidation =<< sendRpcRequest address (D.toRpcRequest arg)

instance L.ControlFlow (Free NetworkingF) where
    delay = evalCoreEffectNetworkingF . L.delay

responseValidation :: (FromJSON b, Applicative f) => Either Text D.RpcResponse -> f (Either Text b)
responseValidation res = case res of
    Left  txt -> pure $ Left txt
    Right (D.RpcResponseError  (A.String txt) _) -> pure $ Left txt
    Right (D.RpcResponseError  err            _) -> pure $ Left (show err)
    Right (D.RpcResponseResult val            _) -> case A.fromJSON val of
        A.Error   txt  -> pure $ Left (Text.pack txt)
        A.Success resp -> pure $ Right resp
