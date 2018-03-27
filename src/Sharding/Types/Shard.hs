{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveGeneric #-}
module Sharding.Types.Shard where

import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Node.Crypto

import              Data.Serialize
import              GHC.Generics

import              Data.Word
import              Data.Serialize
import qualified    Data.ByteString as B




data ShardHash = ShardHash ShardType Word64 Word64 Word64 Word64 Word64 Word64 Word64 Word64
  deriving (Ord, Eq, Show, Generic)

data Shard = Shard ShardType B.ByteString deriving (Ord, Eq, Show, Generic)

data ShardType = ShardType deriving (Ord, Eq, Show, Generic)


instance Serialize ShardHash
instance Serialize Shard
instance Serialize ShardType

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

instance DistanceTo MyNodePosition ShardHash where
    distanceTo (MyNodePosition aNodePosition) aShardHash =
        distance aNodePosition (hashToPoint aShardHash)

shardToHash :: Shard -> ShardHash
shardToHash (Shard aShardType aByteString) =
    case decode $ cryptoHash aByteString of
        Right (x1, x2, x3, x4, x5, x6, x7, x8) ->
            ShardHash aShardType x1 x2 x3 x4 x5 x6 x7 x8
        Left _                                 ->
            error "Sharding.Types.Shard.shardToHash"


distanceNormalizedCapture :: Num a => a
distanceNormalizedCapture = 1024


checkShardIsInRadiusOfCapture :: NodePosition -> Distance Point -> ShardHash -> Bool
checkShardIsInRadiusOfCapture aNodePosition aRadiusOfCapture aShardHashs =
    aShardDistanceToPoint `div` (distanceNormalizedCapture + aShardCaptureDistance) <
        aRadiusOfCapture `div` distanceNormalizedCapture
  where
    aShardDistanceToPoint = distanceTo aNodePosition aShardHashs
    aShardCaptureDistance = shardCaptureDistance aShardHashs
