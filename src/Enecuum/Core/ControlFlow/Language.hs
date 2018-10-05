module Enecuum.Core.ControlFlow.Language where

import           Enecuum.Prelude

data ControlFlowF next where
    Delay :: Int -> (() -> next) -> ControlFlowF next

instance Functor ControlFlowF where
    fmap g (Delay i next) = Delay i (g . next)
  
type ControlFlowL next = Free ControlFlowF next

class ControlFlow m where
    delay :: Int -> m ()

instance ControlFlow (Free ControlFlowF) where
    delay i = liftF $ Delay i id