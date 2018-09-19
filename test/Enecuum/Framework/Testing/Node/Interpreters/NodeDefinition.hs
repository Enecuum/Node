module Enecuum.Framework.Testing.Node.Interpreters.NodeDefinition where

import Enecuum.Prelude

import           Control.Monad.Free
import           Enecuum.Core.Logger.Interpreter

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import           Enecuum.Core.Testing.Runtime.Types

import qualified Enecuum.Framework.Testing.Lens               as RLens
import           Enecuum.Framework.Testing.Types

import           Enecuum.Framework.Testing.Node.Interpreters.NetworkModel
import           Enecuum.Framework.Testing.Node.Interpreters.NodeModel
import           Enecuum.Framework.Testing.Node.Internal.RpcServer

-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL
  :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL rt (L.NodeTag tag next) = do
  L.logInfo $ "Node tag: " +| tag |+ ""
  next <$> (atomically $ writeTVar (rt ^. RLens.tag) tag)
interpretNodeDefinitionL rt (L.EvalNodeModel initScript next) = do
  L.logInfo "Initialization"
  next <$> runNodeModel rt initScript
interpretNodeDefinitionL rt (L.Serving handlersF next) = do
  L.logInfo "Serving handlersF"
  next <$> startNodeRpcServer rt handlersF

interpretNodeDefinitionL _ (L.ServingRpc _ next) = do
  L.logInfo "ServingRpc is undefined"
  undefined

-- | Runs node definition language with node runtime.
runNodeDefinitionL rt = foldFree (interpretNodeDefinitionL rt)
