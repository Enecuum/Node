{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Enecuum.Legacy.Sharding.Types.ShardTypes where

import           Enecuum.Legacy.Service.Types (Hash (..))


import           GHC.Generics

import qualified Data.ByteString              as B
import           Data.Serialize
import           Data.Word
import           Enecuum.Prelude


data ShardHash = ShardHash ShardType Word64 Word64 Word64 Word64 Word64 Word64 Word64 Word64
  deriving (Ord, Eq, Show, Generic)

data Shard = Shard ShardType Hash B.ByteString deriving (Ord, Eq, Show, Generic)

data ShardType = ShardType deriving (Ord, Eq, Show, Generic)

instance Serialize ShardHash
instance Serialize Shard
instance Serialize ShardType
