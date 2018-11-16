{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Core.Crypto.Keys (generateNewRandomAnonymousKeyPair, getPrivateKey, showPublicKey, showPrivateKey,
                                        readPublicKey, readPrivateKey, compressPublicKey, decompressPublicKey,
                                        PublicKey(..), PrivateKey(..), KeyPair (..)) where

import           "cryptonite" Crypto.Hash                (SHA3_256 (..))
import qualified "cryptonite" Crypto.PubKey.ECC.ECDSA    as ECDSA
import           "cryptonite" Crypto.PubKey.ECC.Generate
import           "cryptonite" Crypto.PubKey.ECC.Types
import           "cryptonite" Crypto.Random              (MonadRandom)
import           Data.ByteString.Base58
import qualified Data.ByteString.Char8                   as BC
import           Data.Serialize                          (encode)
import           Enecuum.Prelude
import           Math.NumberTheory.Moduli

newtype PublicKey  = PublicKey256k1 Integer deriving (Show, Read, Generic, Serialize, Eq, Ord, Num, Enum, FromJSON, ToJSON)
newtype PrivateKey = PrivateKey256k1 Integer deriving (Show, Read, Generic, Serialize, Eq, Ord, FromJSON, ToJSON)

deriving instance Ord ECDSA.Signature

-- Point compression on an elliptic curve
compressPublicKey :: ECDSA.PublicKey -> PublicKey
compressPublicKey pub | c == y    = publicKey256k1 (x * 2)
                      | d == y    = publicKey256k1 (x * 2 + 1)
                      | otherwise = error $ "The impossible happened. Can not compress PublicKey " +|| show pub
  where
    (Point x y) = ECDSA.public_q pub
    (c, d)      = curveK (ECDSA.public_curve pub) x

decompressPublicKey :: PublicKey -> ECDSA.PublicKey
decompressPublicKey = getPublicKey . uncompressPublicKey

-- Point decompression on an elliptic curve
uncompressPublicKey :: PublicKey -> (Integer, Integer)
uncompressPublicKey aKey | aa == 0   = (x, c)
                         | otherwise = (x, d)
  where
    k       = fromPublicKey256k1 aKey
    (x, aa) = k `divMod` 2
    (c, d ) = curveK (getCurveByName SEC_p256k1) x

curveK :: Curve -> Integer -> (Integer, Integer)
curveK aCurve x = (c, d)
  where
    (CurveFP (CurvePrime prime (CurveCommon a b _ _ _))) = aCurve
    n = (x ^ (3 :: Int) + x * a + b) `mod` prime
    -- modular square root of @n@ modulo @prime@
    sr = n `sqrtModP` prime
    c = fromMaybe (error $ "The impossible happened. " +|| n ||+ " is a quadratic nonresidue modulo " +|| prime ||+ "") sr
    d = prime - c

publicKey256k1 :: Integer -> PublicKey
publicKey256k1 = PublicKey256k1

fromPublicKey256k1 :: PublicKey -> Integer
fromPublicKey256k1 (PublicKey256k1 i) = i

getPrivateKey :: PrivateKey -> ECDSA.PrivateKey
getPrivateKey (PrivateKey256k1 n) = ECDSA.PrivateKey (getCurveByName SEC_p256k1) n

getPublicKey :: (Integer, Integer) -> ECDSA.PublicKey
getPublicKey (n1, n2) = ECDSA.PublicKey (getCurveByName SEC_p256k1) (Point n1 n2)

data KeyPair    = KeyPair { getPub :: PublicKey, getPriv :: PrivateKey }
  deriving (Show, Read, Generic, Eq, Ord)

readPublicKey :: String -> PublicKey
readPublicKey value = publicKey256k1 $ base58ToInteger value

readPrivateKey :: String -> PrivateKey
readPrivateKey value = PrivateKey256k1 $ base58ToInteger value

showPublicKey :: PublicKey -> String
showPublicKey  a = integerToBase58 $ fromPublicKey256k1 a

showPrivateKey :: PrivateKey -> String
showPrivateKey (PrivateKey256k1 a) = integerToBase58 a

integerToBase58 :: Integer -> String
integerToBase58 = BC.unpack . encodeBase58I bitcoinAlphabet

base58ToInteger :: String -> Integer
base58ToInteger = fromJust . decodeBase58I bitcoinAlphabet . BC.pack

generateNewRandomAnonymousKeyPair :: MonadRandom m => m KeyPair
generateNewRandomAnonymousKeyPair = do
    (pub, priv) <- generate (getCurveByName SEC_p256k1)
    pure $ KeyPair (compressPublicKey pub) (PrivateKey256k1 $ ECDSA.private_d priv)
