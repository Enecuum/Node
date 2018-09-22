{-# LANGUAGE GADTs #-}

module Enecuum.Framework.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D
import qualified Enecuum.Blockchain.Domain.Transaction    as D
import qualified Data.HGraph.THGraph                      as G
import           Data.HGraph.StringHashable               (StringHashable)
import           Enecuum.Core.HGraph.Interpreters.STM     (runHGraphSTM)

-- | Graph types.
type GraphModel g next = Free (L.HGraphL (T.TNodeL g)) next


data GraphAction g x = GraphAction
  { _runner :: forall r. GraphModel g r -> STM r
  , _action :: GraphModel g x
  }

-- | State language.
data StateF next where
  -- | Create variable.
  NewVar :: a -> (D.StateVar a -> next) -> StateF next
  -- | Read variable.
  ReadVar :: D.StateVar a -> (a -> next) -> StateF next
  -- | Write variable.
  WriteVar :: D.StateVar a -> a ->(() -> next) -> StateF next

  MkGraphAction :: GraphAction g x -> (GraphAction g x -> next) -> StateF next
  EvalGraphAction :: GraphAction g x -> (x -> next) -> StateF next
  -- MkGraphAction :: (() -> next) -> StateF next
  -- | Eval graph atomically.
  EvalGraph :: GraphModel g a -> (a -> next) -> StateF next

instance Functor StateF where
  fmap g (NewVar a next)              = NewVar a              (g . next)
  fmap g (ReadVar var next)           = ReadVar var           (g . next)
  fmap g (WriteVar var val next)      = WriteVar var val      (g . next)
  fmap g (MkGraphAction act next)     = MkGraphAction act (g . next)
  fmap g (EvalGraphAction act next)   = EvalGraphAction act (g . next)
  fmap g (EvalGraph graphAction next) = EvalGraph graphAction (g . next)

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


mkGraphAction :: (Serialize g, StringHashable g) => TVar (G.THGraph g) -> GraphModel g a -> StateL (GraphAction g a)
mkGraphAction g graphAction = Free (MkGraphAction x Pure)
  where
    x = GraphAction (runHGraphSTM g) graphAction

evalGraphAction :: GraphAction g a -> StateL a
evalGraphAction graphAction = liftF $ EvalGraphAction graphAction id

-- | Eval graph atomically.
evalGraph :: GraphModel g a -> StateL a
evalGraph graphAction = liftF $ EvalGraph graphAction id
