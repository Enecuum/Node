{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


module Enecuum.Framework.State.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D
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
  Retry :: (() -> next) -> StateF next
  -- | Eval graph atomically.
  EvalGraph :: (Serialize c, T.StringHashable c) => T.TGraph c -> Free (L.HGraphF (T.TNodeL c)) x -> (x -> next) -> StateF next

makeFunctorInstance ''StateF



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
retry :: StateL ()
retry = liftF $ Retry id

-- | Eval graph atomically.
evalGraph
  :: ( T.StringHashable c
     , Serialize c
     ) 
  => T.TGraph c
  -> Free (L.HGraphF (T.TNodeL c)) a
  -> StateL a
evalGraph g act = liftF $ EvalGraph g act id

