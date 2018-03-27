{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
--{-# OPTIONS_GHC -fno-Wtype-defaults #-}

module Sharding.Types.Node where

import              Node.Data.Data

import              Sharding.ShardDB.ShardIndex
import              Sharding.Types.Shard
import              Sharding.Space.Point

import              Lens.Micro.TH
import              Data.Word
import qualified    Data.Set            as S


data ShardingNode = ShardingNode {
        _nodeNeighbors      :: S.Set Neighbor
    ,   _shardingNodeId     :: MyNodeId
    ,   _nodePosition       :: MyNodePosition
    ,   _nodeIndex          :: ShardIndex
    ,   _nodeDistance       :: Word64 -- think
  }
  deriving (Show, Eq, Ord)


data Neighbor = Neighbor {
        _neighborPosition   :: NodePosition
    ,   _neighborId         :: NodeId
  }
  deriving (Show, Eq, Ord)


makeLenses ''ShardingNode
makeLenses ''Neighbor


data ShardingNodeAction =
    -- TODO think requestShardIndex requestNeededShards, find position
    ---    InitAction
        NewNodeInNetAction          NodeId NodePosition
    -- TODO create index for new node by NodeId
    |   ShardIndexCreateAction      NodeId Word64
    |   ShardIndexAcceptAction      [ShardHash]
    |   ShardListCreateAction       NodeId [ShardHash]
    |   ShardAcceptAction           Shard
    ---
    |   NewShardInNetAction         Shard
    |   CleanShardsAction -- clean local Shards
    --- ShiftAction => NewPosiotionResponse
    |   ShiftAction
    |   TheNodeHaveNewCoordinates   NodeId NodePosition
    ---- NeighborListRequest => NeighborListAcceptAction
    |   TheNodeIsDead               NodeId


data ShardingNodeRequestAndResponce =
        IamAwakeRequst        MyNodeId MyNodePosition -- broadcast for all network
    ----
    |   ShardIndexRequest     [NodeId]    -- for neighbors
    |   ShardIndexResponse    NodeId [ShardHash]
    |   ShardListRequest      [ShardHash]
    |   ShardListResponse     NodeId [Shard]
    --- ShiftAction => NewPosiotionResponse
    |   NewPosiotionResponse   MyNodePosition
    ---
    |   NeighborListRequest -- ask net level new neighbors
  deriving (Show)


makeEmptyShardingNode :: S.Set Neighbor ->  MyNodeId -> MyNodePosition -> ShardIndex -> ShardingNode
makeEmptyShardingNode aNeighbors aMyNodeId aMyPosition aMyShardIndex = ShardingNode {
        _nodeNeighbors      = aNeighbors
    ,   _shardingNodeId     = aMyNodeId
    ,   _nodePosition       = aMyPosition
    ,   _nodeIndex          = aMyShardIndex
    ,   _nodeDistance       = 1
  }


makeEmptyNeighbor :: NodePosition -> NodeId -> Neighbor
makeEmptyNeighbor aPosition aNodeId = Neighbor {
        _neighborPosition   = aPosition
    ,   _neighborId         = aNodeId
  }


neighborsMemoryDistanse :: Word64
neighborsMemoryDistanse = 6
