module Enecuum.Framework.Testing.Node.Interpreters.NodeModel where

import Enecuum.Prelude

import           Eff                                ( Eff, Member, handleRelay, runM, send, raise, replaceRelay )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens              as RLens

import           Enecuum.Framework.Testing.Node.Interpreters.NetworkModel
import           Enecuum.Framework.Testing.Node.Interpreters.Networking

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
