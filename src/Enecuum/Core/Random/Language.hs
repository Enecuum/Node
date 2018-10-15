-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Random.Language where

import           Control.Monad.Random                hiding (Random, next)
import qualified Data.ByteString.Internal                          as BSI
import           Enecuum.Blockchain.Domain.Crypto
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

-- | Language for Random.
data ERandomF next where
    -- | Get Int from range
    GetRandomInt :: (Int, Int) -> (Int -> next) -> ERandomF next
    GetRandomByteString :: Int -> ( BSI.ByteString -> next) -> ERandomF next
    EvalRand :: Rand g a -> g -> (a -> next) -> ERandomF next
    GenerateKeyPair :: (KeyPair -> next) -> ERandomF next
    Sign :: (Serialize msg) => PrivateKey -> msg -> (Signature -> next) -> ERandomF next

makeFunctorInstance ''ERandomF

type ERandomL next = Free ERandomF next

class ERandom m where
  getRandomInt :: (Int,Int) -> m Int
  getRandomByteString :: Int -> m BSI.ByteString
  evalRand :: Rand g a -> g -> m a
  generateKeyPair :: m KeyPair
  sign :: (Serialize msg) => PrivateKey -> msg -> m Signature

instance ERandom (Free ERandomF) where
  getRandomInt range = liftF $ GetRandomInt range id
  getRandomByteString k = liftF $ GetRandomByteString k id
  evalRand r g = liftF $ EvalRand r g id
  generateKeyPair = liftF $ GenerateKeyPair id
  sign key msg = liftF $ Sign key msg id
