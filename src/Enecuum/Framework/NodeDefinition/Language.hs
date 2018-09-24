{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

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
data NodeDefinitionF cfg  next where
    -- | Set node tag. For example, "boot node".
    NodeTag        :: D.NodeTag -> (() -> next) -> NodeDefinitionF cfg next
    -- | Evaluate some node model.
    EvalNodeL :: L.NodeL cfg a -> (a -> next) -> NodeDefinitionF cfg next
    -- | Serving of Rpc request.
    ServingRpc     :: PortNumber -> RpcMethodL cfg () -> (() -> next) -> NodeDefinitionF cfg next
    -- | Stop serving of Rpc server.
    StopServing    :: PortNumber -> (() -> next) -> NodeDefinitionF cfg next
    -- | Eval core effect.
    EvalCoreEffectNodeDefinitionF :: L.CoreEffect a -> (a -> next) -> NodeDefinitionF cfg next

instance Functor (NodeDefinitionF cfg) where
    fmap g (NodeTag tag next)               = NodeTag tag               (g . next)
    fmap g (EvalNodeL nodeModel next)       = EvalNodeL nodeModel       (g . next)
    fmap g (ServingRpc port handlersF next) = ServingRpc port handlersF (g . next)
    fmap g (StopServing port next)          = StopServing port          (g . next)
    fmap g (EvalCoreEffectNodeDefinitionF coreEffect next) = EvalCoreEffectNodeDefinitionF coreEffect (g . next)

type NodeDefinitionL cfg next = Free (NodeDefinitionF cfg) next

-- | Sets tag for node.
nodeTag :: D.NodeTag -> NodeDefinitionL cfg ()
nodeTag tag = liftF $ NodeTag tag id

-- | Runs node scenario.
evalNodeL :: L.NodeL cfg a -> NodeDefinitionL cfg a
evalNodeL nodeModel = liftF $ EvalNodeL nodeModel id

-- | Runs RPC server.
servingRpc :: PortNumber -> RpcMethodL cfg () -> NodeDefinitionL cfg ()
servingRpc port handlersF = liftF $ ServingRpc port handlersF id

-- | Eval core effect.
evalCoreEffectNodeDefinitionF :: L.CoreEffect a -> NodeDefinitionL cfg a
evalCoreEffectNodeDefinitionF coreEffect = liftF $ EvalCoreEffectNodeDefinitionF coreEffect id

-- | Runs scenario as initialization.
initialization :: L.NodeL cfg a -> NodeDefinitionL cfg a
initialization = evalNodeL

-- | Runs scenario.
scenario :: L.NodeL cfg a -> NodeDefinitionL cfg a
scenario = evalNodeL

stopServing :: PortNumber -> NodeDefinitionL cfg ()
stopServing port = liftF $ StopServing port id

instance L.Logger (Free (NodeDefinitionF cfg)) where
    logMessage level msg = evalCoreEffectNodeDefinitionF $ L.logMessage level msg
