-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Random.Language where

import           Control.Monad.Random             hiding (Random, next)
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor
-- import           "cryptonite" Crypto.Random (MonadRandom)
import           Enecuum.Blockchain.Domain.Crypto

-- | Language for Random.
data ERandomF next where
    -- | Get integer from range
    GetRandomInt :: (Integer, Integer) -> (Integer -> next) -> ERandomF next
    -- | Eval Rand operation.
    EvalRand :: Rand g a -> g -> (a -> next) -> ERandomF next
    GenerateKeyPair :: (KeyPair -> next) -> ERandomF next
    Sign :: (Serialize msg) => PrivateKey -> msg -> (Signature -> next) -> ERandomF next
    -- EvalMonadRandom ::  (a -> next) -> ERandomF next

makeFunctorInstance ''ERandomF

type ERandomL next = Free ERandomF next

class ERandom m where
  getRandomInt :: (Integer,Integer) -> m Integer
  evalRand :: Rand g a -> g -> m a
  generateKeyPair :: m KeyPair
  sign :: (Serialize msg) => PrivateKey -> msg -> m Signature
  -- evalMonadRandom :: m a
  -- evalMonadRandom :: MonadRandom m => m a

instance ERandom (Free ERandomF) where
  getRandomInt range = liftF $ GetRandomInt range id
  evalRand r g = liftF $ EvalRand r g id
  generateKeyPair = liftF $ GenerateKeyPair id
  sign key msg = liftF $ Sign key msg id
  -- evalMonadRandom = liftF $ EvalMonadRandom id

-- -- -- | Language for Random.
-- data NRandomF next where
--   EvalMonadRandom ::  (a -> next) -> NRandomF next

-- makeFunctorInstance ''NRandomF

-- type NRandomL next = Free NRandomF next

-- class NRandom m where
--   evalMonadRandom :: m a

-- instance NRandom (Free NRandomF) where
--   evalMonadRandom = liftF $ EvalMonadRandom id
