{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveGeneric #-}
module Sharding.Types.ShardLogic where

import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Sharding.Types.ShardTypes
import              Node.Crypto()

import              Data.Serialize


hashToPoint :: ShardHash -> Point
hashToPoint (ShardHash _ x1 x2 _ _ _ _ _ _) = Point x1 x2


class ShardCaptureDistance a where
    shardCaptureDistance :: a -> Distance Point

instance ShardCaptureDistance ShardHash where
    shardCaptureDistance (ShardHash aType _ _ _ _ _ _ _ _) =
        shardCaptureDistance aType

instance ShardCaptureDistance Shard where
    shardCaptureDistance (Shard aType _ _) =
        shardCaptureDistance aType

instance ShardCaptureDistance ShardType where
    shardCaptureDistance _ = 0

instance DistanceTo NodePosition ShardHash where
    distanceTo (NodePosition aNodePosition) aShardHash =
        distance aNodePosition (hashToPoint aShardHash)

instance DistanceTo MyNodePosition ShardHash where
    distanceTo (MyNodePosition aNodePosition) aShardHash =
        distance aNodePosition (hashToPoint aShardHash)

instance DistanceTo MyNodePosition Shard where
    distanceTo (MyNodePosition aNodePosition) aShardHash =
        distance aNodePosition (hashToPoint $ shardToHash aShardHash)

shardToHash :: Shard -> ShardHash
shardToHash (Shard aShardType (Hash aHash) _) =
    case decode aHash of
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
