{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Sharding.Types.Shard where

import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Data.Word
import qualified    Data.ByteString as B


data ShardHash = ShardHash ShardType Word64 Word64 Word64 Word64 Word64 Word64 Word64 Word64
  deriving (Ord, Eq, Show)

data Shard = Shard ShardType B.ByteString deriving (Ord, Eq, Show)

data ShardType = ShardType deriving (Ord, Eq, Show)

hashToPoint :: ShardHash -> Point
hashToPoint (ShardHash _ x1 x2 _ _ _ _ _ _) = Point x1 x2

class ShardCaptureDistance a where
    shardCaptureDistance :: a -> Distance Point

instance ShardCaptureDistance ShardHash where
    shardCaptureDistance (ShardHash aType _ _ _ _ _ _ _ _) =
        shardCaptureDistance aType

instance ShardCaptureDistance Shard where
    shardCaptureDistance (Shard aType _ ) =
        shardCaptureDistance aType

instance ShardCaptureDistance ShardType where
    shardCaptureDistance _ = 0

instance DistanceTo NodePosition ShardHash where
    distanceTo (NodePosition aNodePosition) aShardHash =
        distance aNodePosition (hashToPoint aShardHash)
