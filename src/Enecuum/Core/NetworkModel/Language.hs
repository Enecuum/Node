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

import Enecuum.RuntimeTmp

import           Eff.State                                (State)
import           Eff.SafeIO                               (SIO)
import           Eff.Exc                                  (Exc)
import           Control.Exception                        (SomeException)

-- Low-level network model.

data NetworkSendingL a where
  -- Unicast   :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  -- Broadcast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
  Multicast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()

data NetworkListeningL a where
  WaitForSingleResponse :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL (Maybe D.NetworkResponse)
  -- WaitForResponses      :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL [D.NetworkResponse]

type NetworkSendingModel =
  '[ NetworkSendingL
   , State RuntimeSt
   , SIO
   , Exc SomeException
   ]

type NetworkListeningModel =
  '[ NetworkListeningL
   , State RuntimeSt
   , SIO
   , Exc SomeException
   ]


data NetworkSyncL a where
  Synchronize :: Eff NetworkSendingModel () -> Eff NetworkListeningModel a -> NetworkSyncL a

type NetworkModel =
  '[ NetworkSyncL
   , NetworkListeningL
   , NetworkSendingL
   , State RuntimeSt
   , SIO
   , Exc SomeException
   ]

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