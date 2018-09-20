{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude
import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Networking.Language    as L
import qualified Enecuum.Framework.Domain                 as D

-- | Graph types.
type LGraphModel next = Free (L.HGraphL (T.TNodeL D.Transaction)) next

-- | Node language.
data NodeF next where
    -- | Eval graph.
    EvalGraph      :: LGraphModel a -> (a -> next) -> NodeF next
    -- | Eval networking.
    EvalNetworking :: L.NetworkingL a -> (a -> next) -> NodeF next
    -- | Eval core effect.
    EvalCoreEffectNodeF :: L.CoreEffectModel a -> (a -> next) -> NodeF next

instance Functor NodeF where
    fmap g (EvalGraph graph next) = EvalGraph graph (g . next)
    fmap g (EvalNetworking networking next) =
        EvalNetworking networking (g . next)
    fmap g (EvalCoreEffectNodeF coreEffect next) =
        EvalCoreEffectNodeF coreEffect (g . next)

type NodeModel next = Free NodeF next

-- | Eval graph.
evalGraph :: LGraphModel a -> NodeModel a
evalGraph graph = liftF $ EvalGraph graph id

-- | Eval networking.
evalNetworking :: L.NetworkingL a -> NodeModel a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffectNodeF :: L.CoreEffectModel a -> NodeModel a
evalCoreEffectNodeF coreEffect = liftF $ EvalCoreEffectNodeF coreEffect id

instance L.Logger (Free NodeF) where
    logMessage level msg = evalCoreEffectNodeF $ L.logMessage level msg
