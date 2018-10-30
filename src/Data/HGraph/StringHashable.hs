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
import qualified Crypto.Hash.SHA256         as SHA

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


newtype StringHashSerializable = StringHashSerializable
    { bytes :: [Word8]
    }
    deriving (Generic, ToJSON, FromJSON)

instance ToJSON StringHash where
    toJSON (StringHash bytes) = toJSON $ StringHashSerializable $ BS.unpack bytes

instance FromJSON StringHash where
    parseJSON = A.withObject "StringHashSerializable" $ \v ->
        StringHash . BS.pack <$> ((v A..: "bytes") :: A.Parser [Word8])


-- | integerToHash . hashToInteger === id
--   hashToInteger . integerToHash === abs
hashToInteger :: StringHash -> Integer
hashToInteger x = rollInteger . BS.unpack $ fromRight "" (Base64.decode $ fromStringHash x)

integerToHash :: Integer -> StringHash
integerToHash = StringHash . Base64.encode . BS.pack . unrollInteger

unrollInteger :: Integer -> [Word8]
unrollInteger = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

rollInteger :: [Word8] -> Integer
rollInteger   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b
