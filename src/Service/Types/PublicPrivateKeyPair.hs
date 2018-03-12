{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Service.Types.PublicPrivateKeyPair where

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

--import Math

import              Math.NumberTheory.Moduli
import Data.Int (Int64)
type Amount = Int64


type Signature = ECDSA.Signature

compressPublicKey :: ECDSA.PublicKey -> PublicKey
compressPublicKey pub
    | c == y = publicKey256k1 (x*2)
    | d == y = publicKey256k1 (x*2+1)
    | otherwise = error "error"
  where
    curve = ECDSA.public_curve pub
    (Point x y) = ECDSA.public_q pub
    (CurveFP (CurvePrime prime (CurveCommon a b _ _ _))) = curve
    c = (fromJust $ ((x^(3 :: Int) + x*a + b) `mod` prime) `sqrtModP` prime)
    d = prime - c

uncompressPublicKey :: PublicKey -> (Integer, Integer)
uncompressPublicKey aKey
    | aa == 0   = (x, c)
    | otherwise = (x, d)
  where
    k = fromPublicKey256k1 aKey
    (x, aa)  = k `divMod` 2
    curve = getCurveByName SEC_p256k1
    (CurveFP (CurvePrime prime (CurveCommon a b _ _ _))) = curve
    c = (fromJust $ (( x^(3 :: Int) + x*a + b) `mod` prime ) `sqrtModP` prime)
    d = prime - c

{-
getCompressed :: ECDSA.PublicKey -> PublicKey
getCompressed pub =
 where
-}

publicKey256k1 :: Integer -> PublicKey
publicKey256k1 = PublicKey256k1 . CompactInteger

fromPublicKey256k1 :: PublicKey -> Integer
fromPublicKey256k1 (PublicKey256k1 (CompactInteger i)) = i

newtype PublicKey  = PublicKey256k1 CompactInteger deriving (Generic, Serialize, Eq, Ord)
newtype PrivateKey = PrivateKey256k1 Integer deriving (Generic, Serialize, Eq, Ord)


getPrivateKey :: PrivateKey -> ECDSA.PrivateKey
getPrivateKey  (PrivateKey256k1 n)    =
    ECDSA.PrivateKey (getCurveByName SEC_p256k1) n

getPrivateKey1 :: KeyPair -> ECDSA.PrivateKey
getPrivateKey1 (KeyPair _ privateKey) = getPrivateKey privateKey

getPublicKey :: (Integer, Integer) -> ECDSA.PublicKey
getPublicKey (n1, n2) = ECDSA.PublicKey (getCurveByName SEC_p256k1) (Point n1 n2)
--getPublicKey (PrivateKey256k1 n) = ECDSA.PublicKey (getCurveByName SEC_p256k1)
--   (Point n n)

{-
getPublicKey pubk = ECDSA.PublicKey
  pu@(x1,y1) = uncompressPublicKey pubk
-}

--data KeyPair    = KeyPair PublicKey PrivateKey
data KeyPair    = KeyPair { getPub :: PublicKey, getPriv :: PrivateKey }
  deriving (Show, Generic)

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

instance Show PublicKey where
  show a = "B" ++ if length b == 20 then b else "0" ++ b
   where b = integerToBase58 $ fromPublicKey256k1 a

instance Show PrivateKey where
  show (PrivateKey256k1 a) = "P" ++ if length b == 20 then b else "0" ++ b
   where b = integerToBase58 a

integerToBase58 :: Integer -> [Char]
integerToBase58 = BC.unpack . encodeBase58I bitcoinAlphabet

base58ToInteger :: String -> Integer
base58ToInteger = fromJust . decodeBase58I bitcoinAlphabet . BC.pack

-- showPublicKey :: ECDSA.PublicKey -> String
-- showPublicKey (ECDSA.PublicKey {public_q = Point a b}) = BC.unpack $
--      encodeBase58I rippleAlphabet (pair a b)


{-
-}

-- generateNewRandomAnonymousKeyPair :: IO KeyPair
{-
ppair = unsafePerformIO $ generateNewRandomAnonymousKeyPair
(pub,priv) = ppair
pnt@(Point x y) = public_q pub
curve = public_curve pub
(CurveFP (CurvePrime prime (CurveCommon a b _ _ _))) = curve
-}

{-
PublicKey {public_curve = CurveFP (CurvePrime 4451685225093714772084598273548427
    (CurveCommon {ecc_a = 4451685225093714772084598273548424, ecc_b =
        2061118396808653202902996166388514, ecc_g = Point
            188281465057972534892223778713752
            3419875491033170827167861896082688,
            ecc_n = 4451685225093714776491891542548933, ecc_h = 1})),
            public_q = Point 1853673283466718284163024966200333
                3266228631393728191639932752112443}
-}

generateNewRandomAnonymousKeyPair :: MonadRandom m => m KeyPair
generateNewRandomAnonymousKeyPair = do
    (pub, priv) <- generate (getCurveByName SEC_p256k1)
    pure $ KeyPair (compressPublicKey pub) (PrivateKey256k1 $ ECDSA.private_d priv)


-- | Это более общая версия функции!!!
getSignature :: (Serialize msg, MonadRandom m) => PrivateKey -> msg -> m ECDSA.Signature
getSignature priv msg = ECDSA.sign (getPrivateKey priv) MD2 (encode msg)


-- | Это более общая версия функции!!!
verifySiganture :: Serialize msg => PublicKey -> ECDSA.Signature -> msg -> Bool
verifySiganture pub sign msg = ECDSA.verify MD2 pub' sign (encode msg)
  where pub' = getPublicKey $ uncompressPublicKey pub


test2 :: MonadRandom m => m Bool
test2 = do
    KeyPair pub priv <- generateNewRandomAnonymousKeyPair
    sign <- getSignature priv (23 :: Amount)
    return $ verifySiganture pub sign (23 :: Amount)

byte'msg :: BC.ByteString
byte'msg = BC.pack "32613"

test1 :: MonadRandom m => m Bool
test1 = do
    (KeyPair pub1 priv1)    <- generateNewRandomAnonymousKeyPair
    (KeyPair pub2 _)        <- generateNewRandomAnonymousKeyPair
    sign                    <- ECDSA.sign (getPrivateKey priv1) MD2 byte'msg
    let pub1' = uncompressPublicKey pub1
        _     = uncompressPublicKey pub2
    pure $ ECDSA.verify MD2 (getPublicKey pub1') sign byte'msg
