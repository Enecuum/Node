-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Random.Language where

import           Control.Monad.Random             hiding (Random, next)
import qualified Data.ByteString.Internal         as BSI
import           Enecuum.Core.Crypto.Language     as L
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor
import           Data.UUID (UUID)

-- | Language for Random.
data ERandomF next where
    -- | Eval core crypto effect.
    EvalCoreCrypto :: CryptoL a -> (a -> next) -> ERandomF next
    -- | Get Int from range
    GetRandomInt :: (Int, Int) -> (Int -> next) -> ERandomF next
    -- | Get Random ByteString
    GetRandomByteString :: Int -> ( BSI.ByteString -> next) -> ERandomF next
    -- | create UUID
    NextUUID :: ( UUID -> next ) -> ERandomF next 
makeFunctorInstance ''ERandomF

type ERandomL next = Free ERandomF next

class ERandom m where
    evalCoreCrypto      :: CryptoL a -> m a
    getRandomInt        :: (Int,Int) -> m Int
    getRandomByteString :: Int -> m BSI.ByteString
    nextUUID            :: m UUID

instance ERandom (Free ERandomF) where
    evalCoreCrypto s      = liftF $ EvalCoreCrypto s id
    getRandomInt range    = liftF $ GetRandomInt range id
    getRandomByteString k = liftF $ GetRandomByteString k id
    nextUUID              = liftF $ NextUUID id
