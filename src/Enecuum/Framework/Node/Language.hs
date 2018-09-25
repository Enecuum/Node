{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude
import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Networking.Language    as L
import qualified Enecuum.Framework.Domain                 as D
import qualified Data.HGraph.THGraph                      as G
import           Data.HGraph.StringHashable               (StringHashable)
import           Enecuum.Core.HGraph.Interpreters.IO      (runHGraphIO)
import           Enecuum.Core.HGraph.Interpreters.STM     (runHGraphSTM)

-- | Node language.
data NodeF cfg next where
  -- | Eval stateful action atomically.
  EvalStateAtomically :: L.StateL a -> (a -> next) -> NodeF cfg next
  -- | Eval networking.
  EvalNetworking :: L.NetworkingL cfg a -> (a -> next) -> NodeF cfg next
  -- | Eval core effect.
  EvalCoreEffectNodeF :: L.CoreEffect a -> (a -> next) -> NodeF cfg next
  -- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
  EvalGraphIO :: L.GraphAction g x -> (x -> next) -> NodeF cfg next
  -- | Stop the node evaluation
  StopNode :: (() -> next) -> NodeF cfg next

instance Functor (NodeF cfg) where
  fmap g (EvalStateAtomically statefulAction next) = EvalStateAtomically statefulAction (g . next)
  fmap g (EvalNetworking networking next)          = EvalNetworking networking          (g . next)
  fmap g (EvalCoreEffectNodeF coreEffect next)     = EvalCoreEffectNodeF coreEffect     (g . next)
  fmap g (EvalGraphIO graphAction next)            = EvalGraphIO graphAction            (g . next)
  fmap g (StopNode next)                           = StopNode                           (g . next)

type NodeL cfg next = Free (NodeF cfg) next

-- | Eval stateful action atomically.
evalStateAtomically :: L.StateL a -> NodeL cfg a
evalStateAtomically statefulAction = liftF $ EvalStateAtomically statefulAction id

-- | Alias for convenience.
atomically :: L.StateL a -> NodeL cfg a
atomically = evalStateAtomically

-- | Eval networking.
evalNetworking :: L.NetworkingL cfg a -> NodeL cfg a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffectNodeF :: L.CoreEffect a -> NodeL cfg a
evalCoreEffectNodeF coreEffect = liftF $ EvalCoreEffectNodeF coreEffect id

-- | Stop of node eval.
stopNode :: NodeL cfg ()
stopNode = liftF $ StopNode id

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO
  :: (Serialize g, StringHashable g)
  => TVar (G.THGraph g)
  -> L.HGraphL g a
  -> NodeL cfg a
evalGraphIO g graphAction = liftF $ EvalGraphIO x id
  where
    x = L.GraphAction (runHGraphSTM g) (runHGraphIO g) graphAction

instance L.Logger (Free (NodeF cfg)) where
    logMessage level msg = evalCoreEffectNodeF $ L.logMessage level msg
