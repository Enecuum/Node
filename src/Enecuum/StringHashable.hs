{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enecuum.StringHashable where

import           Universum
import           Data.Serialize

newtype StringHash = StringHash ByteString deriving (Eq, Ord, Serialize)

class Serialize a => StringHashable a where
    toHash :: a -> StringHash

    toHash = StringHash . encode

instance StringHashable Int
instance StringHashable Int64
instance StringHashable Int32
instance StringHashable Int16
instance StringHashable Int8
instance StringHashable Word
instance StringHashable Word64
instance StringHashable Word32
instance StringHashable Word16
instance StringHashable Word8

instance StringHashable StringHash where
    toHash = id