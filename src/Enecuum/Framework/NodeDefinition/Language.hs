{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Framework.Domain                 as D
import           Enecuum.Framework.RpcMethod.Language     (RpcMethodL)
import           Enecuum.Legacy.Service.Network.Base

-- TODO: it's possible to make these steps evaluating step-by-step, in order.
-- Think about if this really needed.

-- | Node description language.
-- Allows to specify what actions should be done when node starts.
data NodeDefinitionF next where
    -- | Set node tag. For example, "boot node".
    NodeTag        :: D.NodeTag -> (() -> next) -> NodeDefinitionF next
    -- | Evaluate some node model.
    EvalNodeModel :: L.NodeModel a -> (a -> next) -> NodeDefinitionF next
    -- | Serving of Rpc request.
    ServingRpc     :: PortNumber -> Free RpcMethodL () -> (() -> next) -> NodeDefinitionF next
    -- | Eval core effect.
    EvalCoreEffectNodeDefinitionF :: L.CoreEffectModel a -> (a -> next) -> NodeDefinitionF next

instance Functor NodeDefinitionF where
    fmap g (NodeTag tag next)               = NodeTag tag               (g . next)
    fmap g (EvalNodeModel nodeModel next)   = EvalNodeModel nodeModel   (g . next)
    fmap g (ServingRpc port handlersF next) = ServingRpc port handlersF (g . next)
    fmap g (EvalCoreEffectNodeDefinitionF coreEffect next) = EvalCoreEffectNodeDefinitionF coreEffect (g . next)

type NodeDefinitionModel next = Free NodeDefinitionF next

-- | Sets tag for node.
nodeTag :: D.NodeTag -> NodeDefinitionModel ()
nodeTag tag = liftF $ NodeTag tag id

-- | Runs node scenario.
evalNodeModel :: L.NodeModel a -> NodeDefinitionModel a
evalNodeModel nodeModel = liftF $ EvalNodeModel nodeModel id

-- | Runs RPC server.
servingRpc :: PortNumber -> Free RpcMethodL () -> NodeDefinitionModel ()
servingRpc port handlersF = liftF $ ServingRpc port handlersF id

-- | Eval core effect.
evalCoreEffectNodeDefinitionF :: L.CoreEffectModel a -> NodeDefinitionModel a
evalCoreEffectNodeDefinitionF coreEffect = liftF $ EvalCoreEffectNodeDefinitionF coreEffect id

-- | Runs scenario as initialization.
initialization :: L.NodeModel a -> NodeDefinitionModel a
initialization = evalNodeModel

-- | Runs scenario.
scenario :: L.NodeModel a -> NodeDefinitionModel a
scenario = evalNodeModel

instance L.Logger (Free NodeDefinitionF) where
    logMessage level msg = evalCoreEffectNodeDefinitionF $ L.logMessage level msg
