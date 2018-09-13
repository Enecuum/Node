module Enecuum.Framework.NetworkModel.Interpreter where

import Enecuum.Prelude

import qualified Data.Aeson                         as A
import qualified Data.ByteString.Lazy               as BS
import           Eff                                ( handleRelay, raise )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import qualified Enecuum.Framework.Lens             as Lens

-- | Interpret NetworkSendingL. Does nothing ATM.
interpretNetworkSendingL
    :: L.NetworkSendingL a
    -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNetworkSendingL (L.Multicast cfg req) = L.logInfo "L.Multicast cfg req"

-- | Interpret NetworkListeningL (with NetworkSendingL in stack). Does nothing ATM.
interpretNetworkListeningL
    :: L.NetworkListeningL a
    -> Eff '[L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkListeningL (L.WaitForSingleResponse cfg timeout) = do
    L.logInfo "L.WaitForSingleResponse cfg timeout"
    pure Nothing

-- | Interpret NetworkListeningL. Does nothing ATM.
interpretNetworkListeningL'
    :: L.NetworkListeningL a
    -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNetworkListeningL' (L.WaitForSingleResponse cfg timeout) = do
    L.logInfo "L.WaitForSingleResponse cfg timeout"
    pure Nothing

-- | Interpret NetworkSyncL. Runs underlying NetworkListeningL and NetworkSendingL interpreters.
interpretNetworkSyncL
    :: L.NetworkSyncL a
    -> Eff '[L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkSyncL (L.Synchronize sending listening) = do
    L.logInfo "Synchronize"
    raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkSendingL  )    sending
    raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkListeningL' ) listening