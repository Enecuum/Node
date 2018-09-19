{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.HGraph.StringHashable where

import           Universum
import           Data.Serialize

newtype StringHash = StringHash ByteString
    deriving (Eq, Ord, Serialize, Show)

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
