{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude

type DBValue = LByteString
type DBKey = LByteString

data DBError
    = KeyNotFound DBKey
    | NotFound Text
    | InvalidType Text
    deriving (Generic, Ord, Eq, Show, Read)

data Storage a