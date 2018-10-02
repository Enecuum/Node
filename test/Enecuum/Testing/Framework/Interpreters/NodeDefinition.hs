module Enecuum.Testing.Framework.Interpreters.NodeDefinition where

import Enecuum.Prelude

import qualified Enecuum.Language as L

import qualified Enecuum.Testing.RLens                        as RLens
import qualified Enecuum.Testing.Types                        as T
import qualified Enecuum.Testing.Core.Interpreters            as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Node  as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State as Impl
import qualified Enecuum.Testing.Framework.Internal.RpcServer as Impl (startNodeRpcServer)
import qualified Enecuum.Testing.Framework.Internal.TcpLikeServer as Impl (startNodeTcpLikeServer)

import qualified Enecuum.Framework.RpcMethod.Interpreter as Impl (runRpcMethodL)
import qualified Enecuum.Framework.MsgHandler.Interpreter as Impl (runMsgHandlerL)

-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL
  :: T.NodeRuntime
  -> L.NodeDefinitionF a
  -> IO a

interpretNodeDefinitionL nodeRt (L.NodeTag tag next) =
  next <$> atomically (writeTVar (nodeRt ^. RLens.tag) tag)

interpretNodeDefinitionL nodeRt (L.EvalNodeL nodeScript next) =
  next <$> Impl.runNodeL nodeRt nodeScript

interpretNodeDefinitionL nodeRt (L.ServingRpc port handlersF next) = do
  methodsMap <- atomically $ newTVar mempty
  Impl.runRpcMethodL methodsMap handlersF
  Impl.startNodeRpcServer nodeRt port methodsMap
  pure $ next ()

interpretNodeDefinitionL nodeRt (L.ServingMsg port handlersF next) = do
  handlersMap <- atomically $ newTVar mempty
  Impl.runMsgHandlerL handlersMap handlersF
  Impl.startNodeTcpLikeServer nodeRt port handlersMap
  pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node definition language with node runtime.
runNodeDefinitionL
  :: T.NodeRuntime
  -> L.NodeDefinitionL a
  -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
