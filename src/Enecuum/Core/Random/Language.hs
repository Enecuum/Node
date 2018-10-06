{-# LANGUAGE GADTs #-}
module Enecuum.Core.Random.Language where

import           Control.Monad.Random hiding (Random, next)
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

-- | Language for Random.
data ERandomF next where
    -- | Get integer from range
    GetRandomInt :: (Integer, Integer) -> (Integer -> next) -> ERandomF next
    -- | Eval Rand operation.
    EvalRand :: Rand g a -> g -> (a -> next) -> ERandomF next

instance Functor ERandomF where
  fmap g (GetRandomInt range next) = GetRandomInt range (g . next)
  fmap g (EvalRand gen a next) = EvalRand gen a (g . next)

type ERandomL next = Free ERandomF next

class ERandom m where
  getRandomInt :: (Integer,Integer) -> m Integer
  evalRand :: Rand g a -> g -> m a

instance ERandom (Free ERandomF) where
  getRandomInt range = liftF $ GetRandomInt range id
  evalRand r g = liftF $ EvalRand r g id
