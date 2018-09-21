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
import qualified Enecuum.Core.Testing.Runtime.Interpreters as Impl
import qualified Enecuum.Framework.Testing.Node.Interpreters.NodeModel as Impl
import           Enecuum.Framework.RpcMethod.Interpreter

-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL
  :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL rt (L.NodeTag tag next) = do
  L.logInfo $ "Node tag: " +| tag |+ ""
  next <$> (atomically $ writeTVar (rt ^. RLens.tag) tag)
interpretNodeDefinitionL rt (L.EvalNodeL initScript next) = do
  L.logInfo "Initialization"
  next <$> runNodeModel rt initScript

interpretNodeDefinitionL nodeRt (L.ServingRpc port handlersF next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "Serving handlersF"
  m <- atomically $ newTVar mempty
  a <- runRpcMethodL m handlersF
  startNodeRpcServer nodeRt port m
  pure $ next a

-- | Runs node definition language with node runtime.
runNodeDefinitionL rt = foldFree (interpretNodeDefinitionL rt)
