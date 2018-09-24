module Enecuum.Framework.Testing.Node.Interpreters.NodeDefinition where

import Enecuum.Prelude

import           Control.Monad.Free                 (foldFree)

import qualified Enecuum.Language                   as L

import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types    (NodeRuntime)

import           Enecuum.Framework.Testing.Node.Internal.RpcServer (startNodeRpcServer)
import qualified Enecuum.Core.Testing.Runtime.Interpreters as Impl
import qualified Enecuum.Framework.Testing.Node.Interpreters.Node as Impl
import           Enecuum.Framework.RpcMethod.Interpreter
import           Enecuum.Framework.Environment

-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionF TestWorld a
  -> IO a

interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
  next <$> (atomically $ writeTVar (nodeRt ^. RLens.tag) tag)

interpretNodeDefinitionL nodeRt (L.EvalNodeL nodeScript next) = do
  next <$> Impl.runNodeL nodeRt nodeScript

interpretNodeDefinitionL nodeRt (L.ServingRpc port handlersF next) = do
  m <- atomically $ newTVar mempty
  a <- runRpcMethodL m handlersF
  startNodeRpcServer nodeRt port m
  pure $ next a

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node definition language with node runtime.
runNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionL TestWorld a
  -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
