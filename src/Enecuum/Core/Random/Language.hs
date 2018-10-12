-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Random.Language where

import           Control.Monad.Random             hiding (Random, next)
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor
import           Enecuum.Blockchain.Domain.Crypto

-- | Language for Random.
data ERandomF next where
    -- | Get Int from range
    GetRandomInt :: (Int, Int) -> (Int -> next) -> ERandomF next
    EvalRand :: Rand g a -> g -> (a -> next) -> ERandomF next
    GenerateKeyPair :: (KeyPair -> next) -> ERandomF next
    Sign :: (Serialize msg) => PrivateKey -> msg -> (Signature -> next) -> ERandomF next

makeFunctorInstance ''ERandomF

type ERandomL next = Free ERandomF next

class ERandom m where
  getRandomInt :: (Int,Int) -> m Int
  evalRand :: Rand g a -> g -> m a
  generateKeyPair :: m KeyPair
  sign :: (Serialize msg) => PrivateKey -> msg -> m Signature

instance ERandom (Free ERandomF) where
  getRandomInt range = liftF $ GetRandomInt range id
  evalRand r g = liftF $ EvalRand r g id
  generateKeyPair = liftF $ GenerateKeyPair id
  sign key msg = liftF $ Sign key msg id
