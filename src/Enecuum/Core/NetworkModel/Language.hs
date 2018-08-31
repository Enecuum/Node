{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.NetworkModel.Language where

import           Eff.TH                                   ( makeFreer )
import           Eff                                      ( Eff
                                                            , Member
                                                            , send
                                                            )
import           GHC.Generics                             ( Generic )

import qualified Enecuum.Core.Types            as T

-- TODO: replace by Interval
type WaitingTimeout = Float

-- This is a raw vision of networking model. Will be updated later.

data NetworkSendingL a where
  -- Unicast   :: T.NetworkConfig -> T.NetworkRequest -> NetworkSendingL ()
  -- Broadcast :: T.NetworkConfig -> T.NetworkRequest -> NetworkSendingL ()
  Multicast :: T.NetworkConfig -> T.NetworkRequest -> NetworkSendingL ()

data NetworkListeningL a where
  WaitForSingleResponse :: T.NetworkConfig -> WaitingTimeout -> NetworkListeningL (Maybe T.NetworkResponse)
  WaitForResponses      :: T.NetworkConfig -> WaitingTimeout -> NetworkListeningL [T.NetworkResponse]

data NetworkSyncL a where
  Synchronize :: Eff '[NetworkSendingL] () -> Eff '[NetworkListeningL] a -> NetworkSyncL a

type NetworkModelL =
  '[ NetworkSendingL
   , NetworkListeningL
   , NetworkSyncL
   ]

makeFreer ''NetworkSendingL
makeFreer ''NetworkListeningL


synchronize = error "synchronize not implemented"


-- waitForSingleResponse
--   :: T.NetworkConfig 
--   -> WaitingTimeout
--   -> Eff '[NetworkListeningL] (Maybe T.NetworkResponse)
-- waitForSingleResponse cfg timeout = send $ WaitForSingleResponse cfg timeout

waitSingleResponse
  :: forall req resp effs
   . T.NetworkMethod () req resp
  => T.NetworkConfig 
  -> WaitingTimeout
  -> (T.NetworkConfig -> T.NetworkRequest -> Eff '[NetworkSendingL] ())
  -> req
  -> Eff effs (Maybe resp)
waitSingleResponse cfg timeout sendingMethodF req =
  synchronize (sendingMethodF cfg $ T.toNetworkRequest () req) (waitForSingleResponse cfg timeout)
