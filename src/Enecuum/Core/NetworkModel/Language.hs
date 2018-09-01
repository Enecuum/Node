{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.NetworkModel.Language where

import           Eff.TH                                   ( makeFreer )
import           Eff                                      ( Eff
                                                            , Member
                                                            , send
                                                            )
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          , O
                                                          , unpack
                                                          )

import qualified Enecuum.Core.Types            as D

-- This is a raw vision of networking model. Will be updated later.

data NetworkSendingL a where
  -- Unicast   :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  -- Broadcast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  Multicast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()

data NetworkListeningL a where
  WaitForSingleResponse :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL (Maybe D.NetworkResponse)
  -- WaitForResponses      :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL [D.NetworkResponse]

data NetworkSyncL a where
  Synchronize :: Eff '[NetworkSendingL] () -> Eff '[NetworkListeningL] a -> NetworkSyncL a

type NetworkModel =
  '[ NetworkSendingL
   , NetworkListeningL
   , NetworkSyncL
   ]

makeFreer ''NetworkSendingL

-- Low-level stuff
waitForSingleResponse
  :: D.NetworkConfig 
  -> D.WaitingTimeout
  -> Eff '[NetworkListeningL] (Maybe D.NetworkResponse)
waitForSingleResponse cfg timeout = send $ WaitForSingleResponse cfg timeout

synchronize
  :: Eff '[NetworkSendingL] ()
  -> Eff '[NetworkListeningL] a 
  -> Eff NetworkModel a
synchronize = error "synchronize not implemented"


-- Interface
waitSingleResponse
  :: D.NetworkConfig 
  -> D.WaitingTimeout
  -> (D.NetworkConfig -> D.NetworkRequest -> Eff '[NetworkSendingL] ())
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