{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Legacy.Sharding.Types.ShardLogic where

import              Enecuum.Legacy.Sharding.Types.ShardTypes
import              Enecuum.Legacy.Node.Crypto()

import              Data.Serialize
import              Enecuum.Legacy.Service.Types (Hash(..))






shardToHash :: Shard -> ShardHash
shardToHash (Shard aShardType (Hash aHash) _) =
    case decode aHash of
        Right (x1, x2, x3, x4, x5, x6, x7, x8) ->
            ShardHash aShardType x1 x2 x3 x4 x5 x6 x7 x8
        Left _                                 ->
            error "Sharding.Types.Shard.shardToHash"

distanceNormalizedCapture :: Num a => a
distanceNormalizedCapture = 1024
