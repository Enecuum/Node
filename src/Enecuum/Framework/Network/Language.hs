{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Network.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D

-- Low-level network model.
-- For now, you can consider it does nothing, and probably we don't need in it at all.

-- Network sending language.

data NetworkSendingF next where
  -- Unicast   :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  -- Broadcast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  Multicast :: D.NetworkConfig -> D.NetworkRequest -> (() -> next) -> NetworkSendingF next

instance Functor NetworkSendingF where
  fmap g (Multicast cfg networkReq next) = Multicast cfg networkReq (g . next)

type NetworkSendingL next = Free NetworkSendingF next

multicast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
multicast cfg req = liftF $ Multicast cfg req id

-- Network listening language.

data NetworkListeningF next where
  WaitForSingleResponse :: D.NetworkConfig -> D.WaitingTimeout -> (Maybe D.NetworkResponse -> next) -> NetworkListeningF next
  -- WaitForResponses      :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL [D.NetworkResponse]

instance Functor NetworkListeningF where
  fmap g (WaitForSingleResponse cfg timeout next) = WaitForSingleResponse cfg timeout (g . next)

type NetworkListeningL next = Free NetworkListeningF next

waitForSingleResponse :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL (Maybe D.NetworkResponse)
waitForSingleResponse cfg timeout = liftF $ WaitForSingleResponse cfg timeout id

-- Network model

data NetworkF next where
  Synchronize :: NetworkSendingL () -> NetworkListeningL a -> (a -> next) -> NetworkF next
  EvalCoreEffectNetworkF :: L.CoreEffect a -> (a -> next) -> NetworkF next

instance Functor NetworkF where
  fmap g (Synchronize sending listening next) = Synchronize sending listening (g . next)
  fmap g (EvalCoreEffectNetworkF coreEffect next) = EvalCoreEffectNetworkF coreEffect (g . next)

type NetworkL next = Free NetworkF next

synchronize :: NetworkSendingL () -> NetworkListeningL a -> NetworkL a
synchronize sending listening = liftF $ Synchronize sending listening id

evalCoreEffectNetworkF :: L.CoreEffect a -> NetworkL a
evalCoreEffectNetworkF coreEffect = liftF $ EvalCoreEffectNetworkF coreEffect id

instance L.Logger (Free NetworkF) where
  logMessage level msg = evalCoreEffectNetworkF $ L.logMessage level msg

-- Low-level stuff

-- Interface
waitSingleResponse
    :: D.NetworkConfig
    -> D.WaitingTimeout
    -> (D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ())
    -> D.NetworkRequest
    -> NetworkL (Maybe D.NetworkResponse)
waitSingleResponse cfg timeout sendingMethodF req =
    synchronize (sendingMethodF cfg req) (waitForSingleResponse cfg timeout)

multicastRequest :: D.NetworkMethod () req resp => D.NetworkConfig -> D.WaitingTimeout -> req -> NetworkL (Maybe resp)
multicastRequest cfg timeout domainRequest = do
    mbResp <- waitSingleResponse cfg timeout multicast (D.toNetworkRequest () domainRequest)
    pure $ mbResp >>= D.fromNetworkResponse ()
