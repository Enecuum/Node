{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as D
import qualified Enecuum.Core.HGraph.Language             as L
import qualified Enecuum.Core.Logger.Language             as L
import           Language.Haskell.TH.MakeFunctor

-- | State language. It reflects STM and its behavior.
data StateF next where
  -- | Create variable.
  NewVar :: a -> (D.StateVar a -> next) -> StateF next
  -- | Read variable.
  ReadVar :: D.StateVar a -> (a -> next) -> StateF next
  -- | Write variable.
  WriteVar :: D.StateVar a -> a -> (() -> next) -> StateF next
  -- | Retry until some variable is changed in this atomic block.
  Retry :: (a -> next) -> StateF next
  -- | Eval graph atomically.
  EvalGraph :: (Serialize c, D.StringHashable c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) x -> (x -> next) -> StateF next
  -- | Eval "delayed" logger: it will be written after successfull state operation.
  EvalDelayedLogger :: L.LoggerL () -> (() -> next) -> StateF next

makeFunctorInstance ''StateF

type StateL = Free StateF

class StateIO m where
  atomically :: StateL a -> m a
  newVarIO :: a -> m (D.StateVar a)
  readVarIO :: D.StateVar a -> m a
  writeVarIO :: D.StateVar a -> a -> m ()

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
retry = liftF $ Retry id

-- | Eval graph atomically.
evalGraph :: (D.StringHashable c, Serialize c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) a -> StateL a
evalGraph g act = liftF $ EvalGraph g act id

-- | Eval "delayed" logger: it will be written after successfull state operation.
evalDelayedLogger :: L.LoggerL () -> StateL ()
evalDelayedLogger action = liftF $ EvalDelayedLogger action id

instance L.Logger StateL where
    logMessage level = evalDelayedLogger . L.logMessage level
