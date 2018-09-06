module Enecuum.Framework.Testing.Runtime.NodeModel.Impl where

import Enecuum.Prelude

import           Eff                                ( Eff, Member, handleRelay, runM, send, raise, replaceRelay )
import           Eff.SafeIO                         ( runSafeIO )

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

interpretNodeL
  :: NodeRuntime
  -> L.NodeL a
  -> Eff '[L.NetworkingL, L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNodeL rt (L.Dummy) = L.logInfo "L.Dummy"

runNodeModel
  :: NodeRuntime
  -> Eff L.NodeModel a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeModel rt
  = handleRelay pure ( (>>=) . interpretNetworkSendingL rt )
  . handleRelay pure ( (>>=) . interpretNetworkListeningL rt )
  . handleRelay pure ( (>>=) . interpretNetworkSyncL rt )
  . handleRelay pure ( (>>=) . interpretNetworkingL rt )
  . handleRelay pure ( (>>=) . interpretNodeL rt )
