{-# LANGUAGE GADTs #-}
module Enecuum.Core.Random.Language where

import           Control.Monad.Random hiding (Random)
import           Enecuum.Prelude

-- | Language for Random.
data ERandomF next where
    -- get integer from range
    GetRandomInt :: (Integer, Integer) -> (Integer -> next) -> ERandomF next
    -- | Generate random value with a predefined type.
    GenRandomValue :: Typeable a => a -> (() -> next) -> ERandomF next
    EvalRand :: Rand g a	-> g -> (a -> next) -> ERandomF next

instance Functor ERandomF where
  fmap g (GetRandomInt i next) = GetRandomInt i (g . next)
  fmap g (GenRandomValue aType next) = GenRandomValue aType (g . next)
  fmap g (EvalRand gen a next) = EvalRand gen a (g . next)

type ERandomL next = Free ERandomF next

class ERandom m where
  getRandomInt :: (Integer,Integer) -> m Integer
  evalRand :: Rand g a -> g -> m a

instance ERandom (Free ERandomF) where
  getRandomInt k = liftF $ GetRandomInt k id
  evalRand r g = liftF $ EvalRand r g id
