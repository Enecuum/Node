module Enecuum.Framework.Testing.Runtime.NetworkModel.Impl where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as BS
import           Control.Monad.IO.Class                                         (MonadIO, liftIO)
import           Control.Monad.Trans.Class                                      (lift)
import           Control.Monad.State.Class                                      (MonadState, get, put)
import           Control.Exception                                              (SomeException)
import           Eff                                                            (Eff, Member, handleRelay, runM, send, raise, replaceRelay)
import           Eff.Exc                                                        (Exc)
import qualified Eff.State                                                      as S
import           Eff.State                                                      (State, get, put)
import           Eff.State.Pure                                                 (evalState)
import           Eff.Reader                                                     (ask)
import           Eff.Reader.Pure                                                (Reader, runReader)
import           Eff.SafeIO                                                     (SIO, runSafeIO, safeIO)
import           Eff.Extra ()

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Framework.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Runtime.STM
import qualified Enecuum.Framework.Testing.Runtime.Lens     as RLens

interpretNetworkSendingL
  :: NodeRuntime
  -> L.NetworkSendingL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNetworkSendingL rt (L.Multicast cfg req) = L.logInfo "L.Multicast cfg req"

interpretNetworkListeningL
  :: NodeRuntime
  -> L.NetworkListeningL a
  -> Eff '[L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkListeningL rt (L.WaitForSingleResponse cfg timeout) = do
  L.logInfo "L.WaitForSingleResponse cfg timeout"
  pure Nothing

interpretNetworkListeningL'
  :: NodeRuntime
  -> L.NetworkListeningL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNetworkListeningL' rt (L.WaitForSingleResponse cfg timeout) = do
  L.logInfo "L.WaitForSingleResponse cfg timeout"
  pure Nothing

interpretNetworkSyncL
  :: NodeRuntime
  -> L.NetworkSyncL a
  -> Eff '[L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkSyncL rt (L.Synchronize sending listening) = do
  L.logInfo "Synchronize"
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkSendingL rt )    sending
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkListeningL' rt ) listening
