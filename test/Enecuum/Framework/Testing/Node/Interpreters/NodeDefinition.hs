module Enecuum.Framework.Testing.Node.Interpreters.NodeDefinition where

import Enecuum.Prelude

import           Eff                                ( Eff, Member, handleRelay, runM, send, raise, replaceRelay )
import           Eff.SafeIO                         ( runSafeIO )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import           Enecuum.Core.Testing.Runtime.Types

import qualified Enecuum.Framework.Testing.Lens               as RLens
import           Enecuum.Framework.Testing.Types

import           Enecuum.Framework.Testing.Node.Interpreters.NetworkModel
import           Enecuum.Framework.Testing.Node.Interpreters.NodeModel
import           Enecuum.Framework.Testing.Node.Internal.RpcServer


interpretNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNodeDefinitionL rt (L.NodeTag tag) = do
  L.logInfo $ "Node tag: " +| tag |+ ""
  safeIO $ atomically $ writeTVar (rt ^. RLens.tag) tag
interpretNodeDefinitionL rt (L.Initialization initScript) = do
  L.logInfo "Initialization"
  runNodeModel rt initScript
interpretNodeDefinitionL rt (L.Serving handlersF) = do
  L.logInfo "Serving handlersF"
  safeIO $ startNodeRpcServer rt handlersF

runNodeDefinitionL
  :: NodeRuntime
  -> Eff '[L.NodeDefinitionL, L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeDefinitionL rt = handleRelay pure ( (>>=) . interpretNodeDefinitionL rt )
