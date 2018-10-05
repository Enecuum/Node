{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.HGraph.StringHashable where

import           Enecuum.Prelude
import           Data.Serialize
import qualified Data.Aeson as A

newtype StringHash = StringHash ByteString
    deriving (Eq, Ord, Serialize, Show, Read, Generic)

class StringHashable a where
    toHash :: a -> StringHash

toHashGeneric :: Serialize a => a -> StringHash
toHashGeneric = StringHash . encode

-- TODO: make it right.
-- Currently, there is no "hashing" in here.
-- https://task.enecuum.com/issues/2718

instance StringHashable Int where toHash = toHashGeneric
instance StringHashable Int64 where toHash = toHashGeneric
instance StringHashable Int32 where toHash = toHashGeneric
instance StringHashable Int16 where toHash = toHashGeneric
instance StringHashable Int8 where toHash = toHashGeneric
instance StringHashable Word where toHash = toHashGeneric
instance StringHashable Word64 where toHash = toHashGeneric
instance StringHashable Word32 where toHash = toHashGeneric
instance StringHashable Word16 where toHash = toHashGeneric
instance StringHashable Word8 where toHash = toHashGeneric

instance StringHashable StringHash where
    toHash = id

fromStringHash :: StringHash -> ByteString
fromStringHash (StringHash sh) = sh


data StringHashSerializable = StringHashSerializable
   { bytes :: Text
   }
  deriving (Generic, ToJSON, FromJSON)

instance ToJSON StringHash where
    toJSON (StringHash bytes) = toJSON $ StringHashSerializable $ show bytes

instance FromJSON StringHash where
    parseJSON obj = undefined
        
