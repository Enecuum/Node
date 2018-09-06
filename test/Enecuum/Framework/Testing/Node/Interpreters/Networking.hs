module Enecuum.Framework.Testing.Node.Interpreters.Networking where

import Enecuum.Prelude

import           Eff                                ( Eff, Member, handleRelay, runM, send, raise, replaceRelay )
import           Eff.SafeIO                         ( runSafeIO )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types

import           Enecuum.Framework.Testing.Node.Interpreters.NetworkModel

interpretNetworkingL
  :: NodeRuntime
  -> L.NetworkingL a
  -> Eff '[L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNetworkingL rt (L.OpenConnection cfg) = do
  L.logInfo "OpenConnection cfg"
  pure $ Just D.Connection
interpretNetworkingL rt (L.CloseConnection conn) = do
  L.logInfo "CloseConnection conn"
  pure ()
interpretNetworkingL rt (L.SendRequest conn req) = do
  L.logInfo "SendRequest conn req"
  pure $ D.RpcResponse ""
interpretNetworkingL rt (L.EvalNetwork networkAction) = do
  L.logInfo "Eval Network"
  networkAction
