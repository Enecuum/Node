{-# LANGUAGE GADTs #-}

module Enecuum.Framework.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D
import qualified Enecuum.Blockchain.Domain.Transaction    as D
import qualified Data.HGraph.THGraph                      as G
import           Data.HGraph.StringHashable               (StringHashable)
import           Enecuum.Core.HGraph.Interpreters.IO      (runHGraphIO)
import           Enecuum.Core.HGraph.Interpreters.STM     (runHGraphSTM)


-- | Wrapper for graph action.
data GraphAction g x = GraphAction
  { _stmRunner :: forall r. L.HGraphL g r -> STM r
  , _ioRunner :: forall r. L.HGraphL g r -> IO r
  , _action :: L.HGraphL g x
  }

-- | State language. It reflects STM and its behavior.
data StateF next where
  -- | Create variable.
  NewVar :: a -> (D.StateVar a -> next) -> StateF next
  -- | Read variable.
  ReadVar :: D.StateVar a -> (a -> next) -> StateF next
  -- | Write variable.
  WriteVar :: D.StateVar a -> a ->(() -> next) -> StateF next
  -- | Retry until some variable is changed in this atomic block.
  Retry :: StateF next
  -- | Eval graph atomically.
  EvalGraph :: GraphAction g x -> (x -> next) -> StateF next

instance Functor StateF where
  fmap g (NewVar a next)         = NewVar a         (g . next)
  fmap g (ReadVar var next)      = ReadVar var      (g . next)
  fmap g (WriteVar var val next) = WriteVar var val (g . next)
  fmap g Retry                   = Retry
  fmap g (EvalGraph act next)    = EvalGraph act    (g . next)

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

-- | Modify variable with function.
modifyVar :: D.StateVar a -> (a -> a) -> StateL ()
modifyVar var f = readVar var >>= writeVar var . f

-- | Retry until some variable is changed in this atomic block.
retry :: StateL a
retry = liftF Retry

-- | Eval graph atomically.
evalGraph
  :: (Serialize g, StringHashable g)
  => TVar (G.THGraph g)
  -> L.HGraphL g a
  -> StateL a
evalGraph g graphAction = liftF $ EvalGraph x id
  where
    x = GraphAction (runHGraphSTM g) (runHGraphIO g) graphAction
