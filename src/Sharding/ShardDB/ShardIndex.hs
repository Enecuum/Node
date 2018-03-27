{-# LANGUAGE TemplateHaskell #-}

module Sharding.ShardDB.ShardIndex where

import              Lens.Micro.TH
import              System.Clock

import              Sharding.Types.Shard
import              Sharding.Space.Distance
import              Sharding.Space.Point

data ShardExistIndex   = ShardExistIndex [SpaceSnapshot]
data ShardNeededIndex  = ShardNeededIndex [ShardHash]
data ShardLoadingIndex = ShardLoadingIndex [(ShardHash, Priority, TimeSpec)]

data Priority          = Priority Int
data SpaceSnapshot     = SpaceSnapshot [(ShardHash, Distance Point)]

data ShardIndex = ShardIndex {
        _shardExistIndex    :: ShardExistIndex
    ,   _shardNeededIndex   :: ShardNeededIndex
    ,   _shardLoadingIndex  :: ShardLoadingIndex
  }

makeLenses ''ShardIndex


addSpaceSnapshot :: SpaceSnapshot -> MyNodePosition -> ShardIndex -> ShardIndex
addSpaceSnapshot aSpaceSnapshot aMyPosition aShardIndex = undefined


addShardToIndex :: Shard -> MyNodePosition -> ShardIndex -> ShardIndex
addShardToIndex aShard aMyPosition aShardIndex = undefined
