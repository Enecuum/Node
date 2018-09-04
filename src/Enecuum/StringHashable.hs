{-# LANGUAGE NoImplicitPrelude #-}

module Enecuum.StringHashable where

import           Universum
import           Data.Serialize

class Serialize a => StringHashable a where
    toHash :: a -> ByteString

    toHash = encode

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
