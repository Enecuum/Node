{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveGeneric #-}
module Sharding.Types.ShardTypes where

import              Sharding.Space.Distance
import              Sharding.Space.Point

import              GHC.Generics

import              Data.Word
import              Data.Serialize
import qualified    Data.ByteString as B

newtype Hash = Hash B.ByteString deriving (Ord, Eq, Show, Generic)

data ShardHash = ShardHash ShardType Word64 Word64 Word64 Word64 Word64 Word64 Word64 Word64
  deriving (Ord, Eq, Show, Generic)

data Shard = Shard ShardType Hash B.ByteString deriving (Ord, Eq, Show, Generic)

data ShardType = ShardType deriving (Ord, Eq, Show, Generic)

instance Serialize ShardHash
instance Serialize Shard
instance Serialize Hash
instance Serialize ShardType
