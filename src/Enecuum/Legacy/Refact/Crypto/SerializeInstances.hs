{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Legacy.Refact.Crypto.SerializeInstances where

import           "cryptonite" Crypto.PubKey.ECC.ECDSA
import           "cryptonite" Crypto.PubKey.ECC.Types
import qualified Data.Aeson                           as A
import           Data.Aeson.TH
import           Data.Bits
import           Data.Hashable
import           Data.Int
import           Data.List                            (unfoldr)
import           Data.Serialize                       (Get, Serialize)
import qualified Data.Serialize                       as S (get, put)
import           Data.Word
import           Enecuum.Prelude


newtype CompactInteger = CompactInteger Integer
    deriving (Eq, Show, Enum, Num, Integral, Real, Ord, Bits, A.ToJSON, A.FromJSON, Hashable)
-- The first number encode length and sign, assuming that we don't have numbers
-- longer that we can encode to 128 byte
instance Serialize CompactInteger where
    put n = do
        let len :: Int8
            len = toEnum ((nrBits (abs n) + 7) `div` 8)
        S.put $ len * (toEnum.fromEnum.signum $ n)
        mapM_ S.put (unroll (abs n))

    get = do
        lenSig <- S.get :: Get Int8
        bytes <- forM [1..abs lenSig] $ \_ -> S.get :: Get Word8
        return $! roll bytes * (toEnum.fromEnum.signum $ lenSig)


--
-- Fold and unfold an Integer to and from a list of its bytes
--

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0 where unstep b a = a `shiftL` 8 .|. fromIntegral b

nrBits :: (Ord a, Integral a) => a -> Int
nrBits k =
    let expMax = until (\e -> 2 ^ e > k) (* 2) 1
        findNr :: Int -> Int -> Int
        findNr lo hi | mid == lo    = hi
                     | 2 ^ mid <= k = findNr mid hi
                     | 2 ^ mid > k  = findNr lo mid
                     | otherwise    = error "Service.Types.SerializeInstances: nrBits"
            where mid = (lo + hi) `div` 2
    in  findNr (expMax `div` 2) expMax


-- tested
instance Serialize Signature where
    put (Signature a b) = S.put (CompactInteger a) *> S.put (CompactInteger b)
    get = do
        CompactInteger a <- S.get
        CompactInteger b <- S.get
        return $ Signature a b

-- automatically get serialization.
deriving instance Generic PublicPoint
deriving instance Generic PublicKey
deriving instance Generic Curve
deriving instance Generic CurveBinary
deriving instance Generic CurvePrime
deriving instance Generic CurveCommon

instance Serialize PublicPoint
instance Serialize PublicKey
instance Serialize Curve
instance Serialize CurveBinary
instance Serialize CurvePrime
instance Serialize CurveCommon

$(deriveJSON defaultOptions ''PublicKey)
$(deriveJSON defaultOptions ''PrivateKey)
$(deriveJSON defaultOptions ''Point)
$(deriveJSON defaultOptions ''Curve)
$(deriveJSON defaultOptions ''CurveBinary)
$(deriveJSON defaultOptions ''CurveCommon)
$(deriveJSON defaultOptions ''CurvePrime)
