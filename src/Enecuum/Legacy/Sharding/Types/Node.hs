{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sharding.Types.Node where

import              Sharding.Types.ShardTypes
import qualified    Control.Concurrent.Chan as C
import              Data.Word
import              Node.Data.Key

data ShardingNodeResponse where
    ShardIndexResponse    :: [ShardHash]    -> ShardingNodeResponse
    ShardResponse         :: [Shard]        -> ShardingNodeResponse
  deriving (Show)


data ShardingNodeRequestMsg =
       NeighborListRequest -- ask net level new neighbors
    |   ShardListRequest            [ShardHash]
  deriving (Show)

data ShardingNodeAction =
        ShardRequestAction          ShardHash (C.Chan Shard)
    |   ShardIndexAcceptAction      [ShardHash]
    |   ShardIndexCreateAction      (C.Chan ShardingNodeResponse) NodeId Word64
    |   ShardLoadAction             (C.Chan ShardingNodeResponse) NodeId ShardHash
    |   NodePositionAction          (C.Chan ShardingNodeResponse) NodeId
    |   ShardAcceptAction           Shard
    |   NewShardInNetAction         Shard
    |   CleanShardsAction
    |   CheckOfShardLoadingList
    |   CleanNeededIndex
    |   CleanRequestIndex
    |   ShardCheckLoading
    |   ShiftAction
    |   CheckTheNeighbors
    |   TheNodeIsDead               NodeId
  deriving Show

instance Show (C.Chan a) where
      show _ = "Chan"
