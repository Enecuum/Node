{-# LANGUAGE
        MultiParamTypeClasses
    ,   DeriveGeneric
  #-}
module Enecuum.Legacy.Sharding.Types.ShardTypes where

import              Enecuum.Legacy.Service.Types (Hash(..))


import              GHC.Generics

import              Data.Word
import              Data.Serialize
import qualified    Data.ByteString as B


data ShardHash = ShardHash ShardType Word64 Word64 Word64 Word64 Word64 Word64 Word64 Word64
  deriving (Ord, Eq, Show, Generic)

data Shard = Shard ShardType Hash B.ByteString deriving (Ord, Eq, Show, Generic)

data ShardType = ShardType deriving (Ord, Eq, Show, Generic)

instance Serialize ShardHash
instance Serialize Shard
instance Serialize ShardType
