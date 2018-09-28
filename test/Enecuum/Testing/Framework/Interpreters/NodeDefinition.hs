module Enecuum.Testing.Framework.Interpreters.NodeDefinition where

import Enecuum.Prelude

import qualified Enecuum.Language as L

import qualified Enecuum.Testing.RLens                        as RLens
import qualified Enecuum.Testing.Types                        as T
import qualified Enecuum.Testing.Core.Interpreters            as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Node  as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State as Impl
import qualified Enecuum.Testing.Framework.Internal.RpcServer as Impl

-- import           Enecuum.Framework.RpcMethod.Interpreter

runRpcMethodL = error "runRpcMethodL not implemented."

-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL
  :: T.NodeRuntime
  -> L.NodeDefinitionF a
  -> IO a

interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
  next <$> (atomically $ writeTVar (nodeRt ^. RLens.tag) tag)

interpretNodeDefinitionL nodeRt (L.EvalNodeL nodeScript next) = do
  next <$> Impl.runNodeL nodeRt nodeScript

interpretNodeDefinitionL nodeRt (L.ServingRpc port handlersF next) = do
  m <- atomically $ newTVar mempty
  a <- runRpcMethodL m handlersF
  Impl.startNodeRpcServer nodeRt port m
  pure $ next a

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node definition language with node runtime.
runNodeDefinitionL
  :: T.NodeRuntime
  -> L.NodeDefinitionL a
  -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
