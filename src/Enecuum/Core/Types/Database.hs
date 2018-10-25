{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude

type DBValueRaw = LByteString
type DBKeyRaw = LByteString

type DBIndex = Int
data DBKey db spec = DBKey LByteString

data DBErrorType
    = KeyNotFound
    | NotFound
    | InvalidType
    deriving (Generic, Ord, Eq, Show, Read)

data DBError = DBError DBErrorType Text

data Storage a
