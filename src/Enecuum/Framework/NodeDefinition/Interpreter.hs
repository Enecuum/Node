module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import qualified Network.WebSockets                 as WS
import qualified Data.Map                           as M
import           Data.Aeson                         as A
import           Control.Concurrent.STM.TChan

import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Refact.Network.Server

import           Enecuum.Framework.Node.Interpreter
import           Enecuum.Framework.RpcMethod.Interpreter
import           Enecuum.Framework.Domain.RPC
import           Enecuum.Framework.Runtime                 (NodeRuntime)

import qualified Enecuum.Framework.Language                as L
import qualified Enecuum.Framework.RLens                   as RLens
import qualified Enecuum.Core.Interpreters                 as Impl
import qualified Enecuum.Framework.Node.Interpreter        as Impl


interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF  a -> IO a
interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNodeL initScript next) =
    next <$> Impl.runNodeL nodeRt initScript

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffect



runNodeDefinitionL :: NodeRuntime -> Free L.NodeDefinitionF a -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
