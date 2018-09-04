module Enecuum.Framework.Testing.Runtime.NodeModel.Impl where

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
import           Enecuum.Framework.Testing.Runtime.NetworkModel.Impl
import qualified Enecuum.Framework.Testing.Runtime.Lens     as RLens

interpretNetworkingL
  :: NodeRuntime
  -> L.NetworkingL a
  -> Eff '[L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkingL rt (L.OpenConnection cfg) = do
  safeIO $ print "OpenConnection cfg"
  pure $ Just D.Connection
interpretNetworkingL rt (L.CloseConnection conn) = do
  safeIO $ print "CloseConnection conn"
  pure ()
interpretNetworkingL rt (L.SendRequest conn req) = do
  safeIO $ print "SendRequest conn req"
  pure D.RpcResponse
interpretNetworkingL rt (L.EvalNetwork networkAction) = do
  safeIO $ print "Eval Network"
  networkAction

interpretNodeL
  :: NodeRuntime
  -> L.NodeL a
  -> Eff '[L.NetworkingL, L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNodeL rt (L.Dummy) = safeIO $ print "L.Dummy"

runNodeL
  :: NodeRuntime
  -> Eff L.NodeModel a
  -> Eff '[SIO, Exc SomeException] a
runNodeL rt
  = handleRelay pure ( (>>=) . interpretNetworkSendingL rt )
  . handleRelay pure ( (>>=) . interpretNetworkListeningL rt )
  . handleRelay pure ( (>>=) . interpretNetworkSyncL rt )
  . handleRelay pure ( (>>=) . interpretNetworkingL rt )
  . handleRelay pure ( (>>=) . interpretNodeL rt )
