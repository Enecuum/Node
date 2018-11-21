{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE TemplateHaskell     #-}

module Enecuum.Core.Crypto.Language where

import           Control.Monad.Random            hiding (Random, next)
import           Data.ByteString.Char8           (pack)
import           Enecuum.Core.Crypto.Crypto
import           Enecuum.Prelude                 hiding (Key)
import           Language.Haskell.TH.MakeFunctor

type Key = ByteString
type Msg = ByteString
type CipheredMsg = ByteString

-- | Language for Cryptography.
data CryptoF next where
    GenerateKeyPair :: (KeyPair -> next) -> CryptoF next
    Sign :: (Serialize msg) => PrivateKey -> msg -> (Signature -> next) -> CryptoF next
    Encrypt :: Key -> Msg -> (CipheredMsg -> next) -> CryptoF next
    Decrypt :: Key -> CipheredMsg -> (Msg -> next) -> CryptoF next
makeFunctorInstance ''CryptoF

type CryptoL next = Free CryptoF next

class Crypto m where
    generateKeyPair :: m KeyPair
    sign :: (Serialize msg) => PrivateKey -> msg -> m Signature
    encrypt :: Key -> Msg -> m CipheredMsg
    decrypt :: Key -> CipheredMsg -> m Msg

instance Crypto (Free CryptoF) where
    generateKeyPair = liftF $ GenerateKeyPair id
    sign key msg    = liftF $ Sign key msg id
    encrypt key msg = liftF $ Encrypt key msg id
    decrypt key secretMsg = liftF $ Decrypt key secretMsg id
