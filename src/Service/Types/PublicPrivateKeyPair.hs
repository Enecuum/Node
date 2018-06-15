{-# LANGUAGE
        PackageImports
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   StandaloneDeriving
    ,   TypeSynonymInstances
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Types.PublicPrivateKeyPair(
        Amount
    ,   ECDSA.Signature
    ,   compressPublicKey
    ,   uncompressPublicKey
    ,   getPublicKey
    ,   PublicKey(..)
    ,   PrivateKey(..)
    ,   KeyPair(..)
    ,   getSignature
    ,   generateNewRandomAnonymousKeyPair
  ) where

import Data.Maybe
import GHC.Generics
import Service.Types.SerializeInstances

import            Data.Serialize
import "cryptonite" Crypto.Random
import "cryptonite" Crypto.PubKey.ECC.Types
import "cryptonite" Crypto.Hash.Algorithms
import "cryptonite" Crypto.PubKey.ECC.Generate
import qualified "cryptonite"  Crypto.PubKey.ECC.ECDSA as ECDSA

import qualified    Data.ByteString.Char8 as BC
import              Data.ByteString.Base58


import              Math.NumberTheory.Moduli
import Data.Int (Int64)

import Text.Read

type Amount = Int64

deriving instance Ord ECDSA.Signature

compressPublicKey :: ECDSA.PublicKey -> PublicKey
compressPublicKey pub
    | c == y = publicKey256k1 (x*2)
    | d == y = publicKey256k1 (x*2+1)
    | otherwise = error "error"
  where
    (Point x y) = ECDSA.public_q pub
    (c, d) = curveK (ECDSA.public_curve pub) x

uncompressPublicKey :: PublicKey -> (Integer, Integer)
uncompressPublicKey aKey
    | aa == 0   = (x, c)
    | otherwise = (x, d)
  where
    k = fromPublicKey256k1 aKey
    (x, aa)  = k `divMod` 2
    (c, d) = curveK (getCurveByName SEC_p256k1) x

curveK :: Curve -> Integer -> (Integer, Integer)
curveK aCurve x = (c, d)
  where
    (CurveFP (CurvePrime prime (CurveCommon a b _ _ _))) = aCurve
    c = fromJust $ (( x^(3 :: Int) + x*a + b) `mod` prime ) `sqrtModP` prime
    d = prime - c

publicKey256k1 :: Integer -> PublicKey
publicKey256k1 = PublicKey256k1 . CompactInteger

fromPublicKey256k1 :: PublicKey -> Integer
fromPublicKey256k1 (PublicKey256k1 (CompactInteger i)) = i

newtype PublicKey  = PublicKey256k1 CompactInteger deriving (Generic, Serialize, Eq, Ord)
newtype PrivateKey = PrivateKey256k1 Integer deriving (Generic, Serialize, Eq, Ord)


getPrivateKey :: PrivateKey -> ECDSA.PrivateKey
getPrivateKey  (PrivateKey256k1 n)    =
    ECDSA.PrivateKey (getCurveByName SEC_p256k1) n

getPublicKey :: (Integer, Integer) -> ECDSA.PublicKey
getPublicKey (n1, n2) = ECDSA.PublicKey (getCurveByName SEC_p256k1) (Point n1 n2)

--data KeyPair    = KeyPair PublicKey PrivateKey
data KeyPair    = KeyPair { getPub :: PublicKey, getPriv :: PrivateKey }
  deriving (Show, Generic)

{-
instance Read PublicKey where
    readsPrec _ ('B':'0':xs) = do
        let v = base58ToInteger xs
        return (publicKey256k1 v, "")
    readsPrec _ xs = error $ "error: readsPrec PublicKey" ++ xs 

instance Read PrivateKey where
    readsPrec _ ('P':'0':xs) = do
        let v = base58ToInteger xs
        return (PrivateKey256k1 v, "")
    readsPrec _ xs = error $ "error: eadsPrec PrivateKey " ++ xs
-}

instance Read PublicKey where
    readPrec = 
        parens
        ( do (Ident s) <- lexP
             case s of 
               ('B':'0':xs) -> do let v = base58ToInteger xs
                                  return (publicKey256k1 v)
               _            -> do error $ "error: readsPrec PublicKey" ++ s  )

instance Read PrivateKey where
    readPrec = 
        parens
        ( do (Ident s) <- lexP
             case s of 
               ('P':'0':xs) -> do let v = base58ToInteger xs
                                  return (PrivateKey256k1 v)
               _            -> do error $ "error: readsPrec PrivateKey" ++ s )

instance Show PublicKey where
  show a = "B" ++ if length b == 20 then b else "0" ++ b
   where b = integerToBase58 $ fromPublicKey256k1 a

instance Show PrivateKey where
  show (PrivateKey256k1 a) = "P" ++ if length b == 20 then b else "0" ++ b
   where b = integerToBase58 a

integerToBase58 :: Integer -> String
integerToBase58 = BC.unpack . encodeBase58I bitcoinAlphabet

base58ToInteger :: String -> Integer
base58ToInteger = fromJust . decodeBase58I bitcoinAlphabet . BC.pack

generateNewRandomAnonymousKeyPair :: MonadRandom m => m KeyPair
generateNewRandomAnonymousKeyPair = do
    (pub, priv) <- generate (getCurveByName SEC_p256k1)
    pure $ KeyPair (compressPublicKey pub) (PrivateKey256k1 $ ECDSA.private_d priv)

-- | Previous version of function was replaced by more generic function
getSignature :: (Serialize msg, MonadRandom m) => PrivateKey -> msg -> m ECDSA.Signature
getSignature priv msg = ECDSA.sign (getPrivateKey priv) MD2 (encode msg)
