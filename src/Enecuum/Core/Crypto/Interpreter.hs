{-# LANGUAGE PackageImports #-}
module Enecuum.Core.Crypto.Interpreter where

import           "cryptonite" Crypto.Random (MonadRandom)
import           Crypto.TripleSec           (decryptIO, encryptIO)
import           Data.ByteString.Char8      (pack)
import           Enecuum.Core.Crypto.Crypto (generateNewRandomAnonymousKeyPair, sign)
import           Enecuum.Core.Crypto.Crypto
import qualified Enecuum.Core.Language      as L
import           Enecuum.Prelude

interpretCryptoL :: L.CryptoF a -> IO a
interpretCryptoL (L.GenerateKeyPair next) =
    next <$> generateNewRandomAnonymousKeyPair
interpretCryptoL (L.Sign key msg next) = do
    signature <- sign key msg
    pure $ next signature
interpretCryptoL (L.Encrypt key msg next) = do
    encryptedMsg <- encryptIO key msg
    pure $ next encryptedMsg
interpretCryptoL (L.Decrypt key encryptedMsg next) = do
    decryptedMsg <- decryptIO key encryptedMsg
    pure $ next decryptedMsg

runCryptoL :: L.CryptoL a -> IO a
runCryptoL = foldFree interpretCryptoL





