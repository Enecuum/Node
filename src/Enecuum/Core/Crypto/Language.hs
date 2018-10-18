-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Crypto.Language where

import           Control.Monad.Random                hiding (Random, next)
import qualified Data.ByteString.Internal                          as BSI
import           Enecuum.Blockchain.Domain.Crypto
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

-- | Language for Cryptography.
data CryptoF next where
  GenerateKeyPair :: (KeyPair -> next) -> CryptoF next
  Sign :: (Serialize msg) => PrivateKey -> msg -> (Signature -> next) -> CryptoF next

makeFunctorInstance ''CryptoF

type CryptoL next = Free CryptoF next

class Crypto m where
  generateKeyPair :: m KeyPair
  sign :: (Serialize msg) => PrivateKey -> msg -> m Signature

instance Crypto (Free CryptoF) where
  generateKeyPair = liftF $ GenerateKeyPair id
  sign key msg = liftF $ Sign key msg id
