module Enecuum.Framework.Testing.Node.Interpreters.NetworkModel where

import Enecuum.Prelude

import qualified Data.Aeson                         as A
import qualified Data.ByteString.Lazy               as BS
import           Eff                                ( handleRelay, raise )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import qualified Enecuum.Framework.Lens             as Lens
import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types

-- | Interpret NetworkSendingL. Does nothing ATM.
interpretNetworkSendingL
  :: NodeRuntime
  -> L.NetworkSendingL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNetworkSendingL rt (L.Multicast cfg req) = L.logInfo "L.Multicast cfg req"

-- | Interpret NetworkListeningL (with NetworkSendingL in stack). Does nothing ATM.
interpretNetworkListeningL
  :: NodeRuntime
  -> L.NetworkListeningL a
  -> Eff '[L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkListeningL rt (L.WaitForSingleResponse cfg timeout) = do
  L.logInfo "L.WaitForSingleResponse cfg timeout"
  pure Nothing

-- | Interpret NetworkListeningL. Does nothing ATM.
interpretNetworkListeningL'
  :: NodeRuntime
  -> L.NetworkListeningL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNetworkListeningL' rt (L.WaitForSingleResponse cfg timeout) = do
  L.logInfo "L.WaitForSingleResponse cfg timeout"
  pure Nothing

-- | Interpret NetworkSyncL. Runs underlying NetworkListeningL and NetworkSendingL interpreters.
interpretNetworkSyncL
  :: NodeRuntime
  -> L.NetworkSyncL a
  -> Eff '[L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkSyncL rt (L.Synchronize sending listening) = do
  L.logInfo "Synchronize"
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkSendingL rt )    sending
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkListeningL' rt ) listening
