{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Core.Database.Types where

import           Enecuum.Prelude

type DBValue = LByteString
type DBKey = LByteString

data DBError
    = KeyNotFound DBKey
    | InvalidType Text
    deriving (Generic, Ord, Eq, Show, Read, ToJSON, FromJSON)
