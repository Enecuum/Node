{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Legacy.Sharding.Types.Node where

import qualified Control.Concurrent.Chan                  as C
import           Data.Word
import           Enecuum.Legacy.Node.Data.Key
import           Enecuum.Legacy.Sharding.Types.ShardTypes
import           Prelude                                  (show)
import           Universum

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
