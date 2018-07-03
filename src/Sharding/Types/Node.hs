{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, GADTs #-}
--{-# OPTIONS_GHC -fno-Wtype-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sharding.Types.Node where

import              Sharding.ShardDB.ShardIndex
import              Sharding.Types.ShardTypes

import              Control.Concurrent.Chan.Unagi.Bounded
import qualified    Control.Concurrent.Chan as C

import              Lens.Micro.TH
import              Data.Word
import qualified    Data.Set            as S
import qualified    Data.Map            as M
import              System.Clock
import              Service.InfoMsg
import              Node.Data.Key


data ShardingNodeResponse where
    ShardIndexResponse    :: [ShardHash]    -> ShardingNodeResponse
    ShardResponse         :: [Shard]        -> ShardingNodeResponse
--    NodePositionResponse  :: MyNodePosition -> ShardingNodeResponse
  deriving (Show)


data ShardingNodeRequestMsg =
--        IamAwakeRequest              MyNodeId MyNodePosition -- broadcast for all network
       NeighborListRequest -- ask net level new neighbors
--    |   ShardIndexRequest           Word64 [NodePosition]
    |   ShardListRequest            [ShardHash]
    --  ShiftAction => NewPosiotionResponse
--    |   NewPosiotionMsg             MyNodePosition
--    |   IsTheNeighborAliveRequest   NodeId NodePosition
  deriving (Show)


{-

data ShardingNode = ShardingNode {
        _nodeNeighbors      :: S.Set Neighbor
    ,   _shardingNodeId     :: MyNodeId
    ,   _nodeIndexOfReques  :: M.Map ShardHash (TimeSpec, C.Chan Shard)
    ,   _nodeInfoMsgChan    :: InChan InfoMsg
    ,   _nodeDistance       :: Word64 -- think
  }
  deriving Eq


data Neighbor = Neighbor {
      _neighborId         :: NodeId
  }
  deriving (Show, Eq, Ord)


makeLenses ''ShardingNode
makeLenses ''Neighbor


data ShardingNodeAction =
        ShardRequestAction          ShardHash (C.Chan Shard)
    |   ShardIndexAcceptAction      [ShardHash]
    |   ShardIndexCreateAction      (C.Chan ShardingNodeResponse) NodeId Word64
    |   ShardLoadAction             (C.Chan ShardingNodeResponse) NodeId ShardHash
    |   NodePositionAction          (C.Chan ShardingNodeResponse) NodeId
    |   ShardAcceptAction           Shard
    ---
    |   NewShardInNetAction         Shard
    |   NeighborListAcceptAction    [(NodeId, NodePosition)]
    |   CleanShardsAction
    |   CheckOfShardLoadingList
    |   CleanNeededIndex
    |   CleanRequestIndex
    |   ShardCheckLoading
    --- ShiftAction => NewPosiotionResponse
    |   ShiftAction
    |   CheckTheNeighbors
    |   TheNodeHaveNewCoordinates   NodeId NodePosition
    ---- NeighborListRequest => NeighborListAcceptAction
    --  BUG the generation of TheNodeIsDead from net lvl.
    |   TheNodeIsDead               NodeId
  deriving Show

instance Show (C.Chan a) where
      show _ = "Chan"




makeEmptyShardingNode :: S.Set Neighbor ->  MyNodeId -> MyNodePosition -> ShardIndex -> InChan InfoMsg -> Word64 -> ShardingNode
makeEmptyShardingNode aNeighbors aMyNodeId aMyPosition aMyShardIndex infoMsgChan aNodeDistance = ShardingNode {
        _nodeNeighbors      = aNeighbors
    ,   _shardingNodeId     = aMyNodeId
    ,   _nodePosition       = aMyPosition
    ,   _nodeIndex          = aMyShardIndex
    ,   _nodeInfoMsgChan    = infoMsgChan
    ,   _nodeIndexOfReques  = M.empty
    ,   _nodeDistance       = aNodeDistance
  }


makeEmptyNeighbor :: NodePosition -> NodeId -> Neighbor
makeEmptyNeighbor aPosition aNodeId = Neighbor aNodeId


neighborsMemoryDistanse :: Word64
neighborsMemoryDistanse = 6
-}
