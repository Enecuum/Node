{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
--{-# OPTIONS_GHC -fno-Wtype-defaults #-}

module Sharding.Types where

import              Node.Data.Data

import              Lens.Micro.TH
import              Sharding.Space.Points
import              Sharding.Space.Distance

import              Data.Word
import qualified    Data.ByteString     as B
import qualified    Data.Set            as S


--------------------------------CONSTANTS---------------------------------------

neighborsDistanseMemoryConstant :: Word64
neighborsDistanseMemoryConstant = 6

distanceNormalizedConstant :: Word64
distanceNormalizedConstant = 1024

--------------------------------------------------------------------------------


data ShardHash = ShardHash ShardType Word64 Word64 Word64 Word64 Word64 Word64 Word64 Word64
  deriving (Ord, Eq, Show)

data Shard = Shard ShardType B.ByteString deriving (Ord, Eq, Show)

data ShardType = ShardType deriving (Ord, Eq, Show)

data ShardingNode = ShardingNode {
        _nodeNeighbors      :: S.Set Neighbor
    ,   _shardingNodeId     :: MyNodeId
    ,   _nodePosition       :: MyNodePosition
    ,   _nodeIndex          :: S.Set ShardHash
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
    |   ShardsAcceptAction          [(ShardHash, Shard)]
    ---
    |   NewShardInNetAction         ShardHash Shard
    |   CleanShardsAction -- clean local Shards
    --- ShiftAction => NewPosiotionResponse
    |   ShiftAction
    |   TheNodeHaveNewCoordinates   NodeId NodePosition
    ---- NeighborListRequest => NeighborListAcceptAction
    |   NeighborListAcceptAction   [(NodeId, NodePosition)]
    |   TheNodeIsDead               NodeId


data ShardingNodeRequestAndResponce =
        IamAwakeRequst        MyNodeId MyNodePosition -- broadcast for all network
    ----
    |   ShardIndexRequest     [NodeId]    -- for neighbors
    |   ShardIndexResponse    NodeId [ShardHash]
    |   ShardListRequest      [ShardHash]
    |   ShardListResponse     NodeId [(ShardHash, Shard)]
    --- ShiftAction => NewPosiotionResponse
    |   NewPosiotionResponse   MyNodePosition
    ---
    |   NeighborListRequest -- ask net level new neighbors
  deriving (Show)
--
hashToPoint :: ShardHash -> Point
hashToPoint (ShardHash _ x1 x2 _ _ _ _ _ _) = Point x1 x2


makeEmptyShardingNode :: S.Set Neighbor ->  MyNodeId -> MyNodePosition -> S.Set ShardHash -> ShardingNode
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
