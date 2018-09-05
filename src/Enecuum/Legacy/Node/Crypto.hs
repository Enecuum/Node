{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Enecuum.Legacy.Node.Crypto where

import           Enecuum.Legacy.Node.Data.Key hiding (PublicKey (..))

import           Crypto.Cipher.Types
import           Crypto.Error
import           "cryptonite" Crypto.Hash
import           Crypto.PubKey.ECC.DH
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.Random.Types

import           Crypto.Cipher.AES            (AES256)
import           Data.ByteArray               (unpack)

import           Data.ByteString              (ByteString, pack)
import           Data.Serialize
import           Enecuum.Prelude

verifyEncodeble :: Serialize msg => PublicKey -> Signature -> msg -> Bool
verifyEncodeble aPublicKey aSignature aMsg = verify SHA3_256
    aPublicKey aSignature (encode aMsg)

signEncodeble :: (MonadRandom m, Serialize msg) =>
    PrivateKey
    -> msg
    -> m Signature
signEncodeble aPrivateKey aMsg = sign aPrivateKey SHA3_256 (encode aMsg)

genKeyPair :: MonadRandom m => Curve -> m (PrivateNumber, PublicPoint)
genKeyPair cur = do
    aPrivateKey <- generatePrivate cur
    pure (aPrivateKey, calculatePublic cur aPrivateKey)


encrypt :: StringKey -> ByteString -> CryptoFailable ByteString
encrypt (StringKey secretKey) aMsg = do
    cipher :: AES256 <- cipherInit secretKey
    return $ ctrCombine cipher nullIV aMsg


cryptoHash :: Serialize a => a -> ByteString
cryptoHash = pack . unpack . fastHash

fastHash :: Serialize a => a -> Digest SHA3_512
fastHash bs = hash $ encode bs :: Digest SHA3_512
