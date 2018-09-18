{-# LANGUAGE GADTs #-}

module Enecuum.Framework.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D

-- | Graph types.
type LGraphModel next = Free (L.HGraphL (T.TNodeL D.Transaction)) next

-- | State language.
data StateF next where
  -- | Create variable.
  NewVar :: a -> (D.StateVar a -> next) -> StateF next
  -- | Read variable.
  ReadVar :: D.StateVar a -> (a -> next) -> StateF next
  -- | Write variable.
  WriteVar :: D.StateVar a -> a ->(() -> next) -> StateF next
  -- | Eval graph.
  EvalGraph :: LGraphModel a -> (a -> next) -> StateF next
  -- | Eval core effect.
  EvalCoreEffectStateF :: L.CoreEffectModel a -> (a -> next) -> StateF next

instance Functor StateF where
  fmap g (NewVar a next)                        = NewVar a                        (g . next)
  fmap g (ReadVar var next)                     = ReadVar var                     (g . next)
  fmap g (WriteVar var val next)                = WriteVar var val                (g . next)
  fmap g (EvalGraph graph next)                 = EvalGraph graph                 (g . next)
  fmap g (EvalCoreEffectStateF coreEffect next) = EvalCoreEffectStateF coreEffect (g . next)

type StateL next = Free StateF next

-- | Create variable.
newVar :: a -> StateL (D.StateVar a)
newVar val = liftF $ NewVar val id

-- | Read variable.
readVar :: D.StateVar a -> StateL a
readVar var = liftF $ ReadVar var id

-- | Write variable.
writeVar :: D.StateVar a -> a -> StateL ()
writeVar var val = liftF $ WriteVar var val id

-- | Eval graph.
evalGraph :: LGraphModel a -> StateL a
evalGraph graph = liftF $ EvalGraph graph id

-- | Eval core effect.
evalCoreEffectStateF :: L.CoreEffectModel a -> StateL a
evalCoreEffectStateF coreEffect = liftF $ EvalCoreEffectStateF coreEffect id

instance L.Logger (Free StateF) where
  logMessage level msg = evalCoreEffectStateF $ L.logMessage level msg
