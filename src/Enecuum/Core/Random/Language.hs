{-# LANGUAGE GADTs #-}
module Enecuum.Core.Random.Language where

import           Control.Monad.Random
import           Enecuum.Prelude

-- | Language for Random.
data RandomF next where
    -- | Generate random value with a predefined type.
    GenRandomValue :: Typeable a => a -> (() -> next) -> RandomF next
    EvalRand :: Rand g a	-> g -> (a -> next) -> RandomF next

instance Functor RandomF where
  fmap g (GenRandomValue aType next) = GenRandomValue aType (g . next)
  fmap g (EvalRand gen a next) = EvalRand gen a (g . next)

type RandomL next = Free RandomF next

class CRandom m where
  evalRand :: Rand g a -> g -> m a

instance CRandom (Free RandomF) where
  evalRand r g = liftF $ EvalRand r g id