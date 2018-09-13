{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.NetworkModel.Language where

import           Enecuum.Prelude

import           Eff                                      ( send )

import           Enecuum.Core.Language                    ( CoreEffects )
import qualified Enecuum.Framework.Domain                 as D

-- Low-level network model.
-- For now, you can consider it does nothing, and probably we don't need in it at all.

data NetworkSendingL a where
  -- Unicast   :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  -- Broadcast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  Multicast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()

data NetworkListeningL a where
  WaitForSingleResponse :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL (Maybe D.NetworkResponse)
  -- WaitForResponses      :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL [D.NetworkResponse]

type NetworkSendingModel =
  '[ NetworkSendingL
   ]
  ++ CoreEffects

type NetworkListeningModel =
  '[ NetworkListeningL
   ]
  ++ CoreEffects


data NetworkSyncL a where
  Synchronize :: Eff NetworkSendingModel () -> Eff NetworkListeningModel a -> NetworkSyncL a

type NetworkModel =
  '[ NetworkSyncL
   , NetworkListeningL
   , NetworkSendingL
   ]
  ++ CoreEffects

makeFreer ''NetworkSendingL

-- Low-level stuff
waitForSingleResponse
  :: D.NetworkConfig
  -> D.WaitingTimeout
  -> Eff NetworkListeningModel (Maybe D.NetworkResponse)
waitForSingleResponse cfg timeout = send $ WaitForSingleResponse cfg timeout

synchronize
  :: Eff NetworkSendingModel ()
  -> Eff NetworkListeningModel a
  -> Eff NetworkModel a
synchronize s l = send $ Synchronize s l


-- Interface
waitSingleResponse
  :: D.NetworkConfig
  -> D.WaitingTimeout
  -> (D.NetworkConfig -> D.NetworkRequest -> Eff NetworkSendingModel ())
  -> D.NetworkRequest
  -> Eff NetworkModel (Maybe D.NetworkResponse)
waitSingleResponse cfg timeout sendingMethodF req =
  synchronize (sendingMethodF cfg req) (waitForSingleResponse cfg timeout)

multicastRequest
  :: D.NetworkMethod () req resp
  => D.NetworkConfig
  -> D.WaitingTimeout
  -> req
  -> Eff NetworkModel (Maybe resp)
multicastRequest cfg timeout domainRequest = do
  mbResp <- waitSingleResponse cfg timeout multicast (D.toNetworkRequest () domainRequest)
  pure $ mbResp >>= D.fromNetworkResponse ()
