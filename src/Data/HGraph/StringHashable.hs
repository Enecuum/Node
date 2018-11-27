{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
--{-# LANGUAGE DerivingVia                #-}

module Data.HGraph.StringHashable where

import           Enecuum.Prelude
import           Data.Serialize
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import           Data.Bits
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.Text.Encoding         as T

newtype StringHash = StringHash ByteString
    deriving (Eq, Ord, Show, Read, Generic, Serialize)

class StringHashable a where
    toHash :: a -> StringHash

toHashGeneric :: Serialize a => a -> StringHash
toHashGeneric = StringHash . Base64.encode . SHA.hash . encode

-- TODO: make it right.
-- Currently, there is no "hashing" in here.
-- https://task.enecuum.com/issues/2718

instance StringHashable Integer where toHash = toHashGeneric
instance StringHashable Int     where toHash = toHashGeneric
instance StringHashable Int64   where toHash = toHashGeneric
instance StringHashable Int32   where toHash = toHashGeneric
instance StringHashable Int16   where toHash = toHashGeneric
instance StringHashable Int8    where toHash = toHashGeneric
instance StringHashable Word    where toHash = toHashGeneric
instance StringHashable Word64  where toHash = toHashGeneric
instance StringHashable Word32  where toHash = toHashGeneric
instance StringHashable Word16  where toHash = toHashGeneric
instance StringHashable Word8   where toHash = toHashGeneric


instance StringHashable StringHash where
    toHash = id

fromStringHash :: StringHash -> ByteString
fromStringHash (StringHash sh) = sh


instance ToJSON StringHash where
    toJSON (StringHash bytes) = toJSON $ T.decodeUtf8 bytes

instance FromJSON StringHash where
    parseJSON (A.String v) = pure $ StringHash (T.encodeUtf8 v)
    parseJSON _ = mzero


-- | integerToHash . hashToInteger === id
--   hashToInteger . integerToHash === abs
hashToInteger :: StringHash -> Integer
hashToInteger x = rollInteger . BS.unpack $ fromRight "" (Base64.decode $ fromStringHash x)

integerToHash :: Integer -> StringHash
integerToHash = StringHash . Base64.encode . BS.pack . unrollInteger

hashToWord64 :: StringHash -> Word64
hashToWord64 x = fromIntegral $ hashToInteger x `div` 2^64

unrollInteger :: Integer -> [Word8]
unrollInteger = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

rollInteger :: [Word8] -> Integer
rollInteger   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b
