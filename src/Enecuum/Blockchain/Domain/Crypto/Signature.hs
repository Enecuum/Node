{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Blockchain.Domain.Crypto.Signature where

import           "cryptonite" Crypto.Hash.Algorithms   (SHA3_256 (..))
import qualified "cryptonite" Crypto.PubKey.ECC.ECDSA  as ECDSA
import           "cryptonite" Crypto.Random            (MonadRandom)
import           Data.Aeson                            ((.:), (.=) )
import qualified Data.Aeson                            as A
import           Data.Aeson.Types                      (typeMismatch)
import           Data.Bits
import           Data.ByteString.Base64.Extra
import qualified Data.ByteString.Char8                 as BS
import           Data.ByteString.Conversion
import           Data.ByteString.Extra
import           Data.Serialize                        (Serialize, encode)
import           Data.Serialize                        (Get, Serialize)
import qualified Data.Serialize                        as S
import qualified Data.Serialize                        as S (get, put)
import qualified Data.Text                             as E (Text, pack, unpack)
import qualified Enecuum.Blockchain.Domain.Crypto.Keys as Enq
import           Enecuum.Prelude                       hiding (pack, unpack, (.=))

sign :: (Serialize msg, MonadRandom m) => Enq.PrivateKey -> msg -> m ECDSA.Signature
sign priv msg = ECDSA.sign (Enq.getPrivateKey priv) SHA3_256 (S.encode msg)



intToBase64Text :: Integer -> Text
intToBase64Text i = base64ToText $ show i

base64TextToInt :: (MonadPlus m) => Text -> m Integer
base64TextToInt b = do
    bs <- textToBase64 b
    case fromByteString bs of
        Just i -> pure i
        _      -> mzero


instance ToJSON ECDSA.Signature where
  toJSON t = A.object [
    "sign_r" .= intToBase64Text  (ECDSA.sign_r t),
    "sign_s" .= intToBase64Text  (ECDSA.sign_s t) ]

instance FromJSON ECDSA.Signature where
  parseJSON (A.Object v) = do
    s_r <- base64TextToInt =<< v .: "sign_r"
    s_s <- base64TextToInt =<< v .: "sign_s"
    pure $ ECDSA.Signature s_r s_s
  parseJSON inv        = typeMismatch "Signature" inv

newtype CompactInteger = CompactInteger Integer deriving (Eq, Show, Enum, Num, Integral, Real, Ord, Bits, A.ToJSON, A.FromJSON, Hashable)
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
      pure $! roll bytes * (toEnum.fromEnum.signum $ lenSig)


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
instance Serialize ECDSA.Signature where
  put (ECDSA.Signature a b) = S.put (CompactInteger a) *> S.put (CompactInteger b)
  get = do
      CompactInteger a <- S.get
      CompactInteger b <- S.get
      pure $ ECDSA.Signature a b
