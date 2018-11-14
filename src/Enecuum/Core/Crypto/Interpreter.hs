{-# LANGUAGE PackageImports             #-}
module Enecuum.Core.Crypto.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language     as L
-- import qualified Control.Monad.Random.Lazy as R
import           "cryptonite" Crypto.Random (MonadRandom)
import Enecuum.Core.Crypto.Crypto (generateNewRandomAnonymousKeyPair, sign)



-- | Interpret CryptoL language.
interpretCryptoL :: MonadRandom m => L.CryptoF a -> m a
interpretCryptoL (L.GenerateKeyPair next) =
    next <$> generateNewRandomAnonymousKeyPair
interpretCryptoL (L.Sign key msg next) = do
    signature <- sign key msg
    pure $ next signature

runCryptoL :: MonadRandom m => L.CryptoL a -> m a
runCryptoL = foldFree interpretCryptoL





