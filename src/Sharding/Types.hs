module Sharding.Types where

import              Node.Data.Data

import              Sharding.Space.Points
import              Sharding.Space.Distance

import              Data.Word
import qualified    Data.ByteString     as B
import qualified    Data.Set            as S

type BlockHash = (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64)

data Block = Block BlockHash B.ByteString deriving (Ord, Eq, Show)

data ShardingNode = ShardingNode {
        nodeNeighbors           :: S.Set Neighbor
    ,   shardingNodeId          :: MyNodeId
    ,   nodePosition            :: MyNodePosition
    ,   nodeIndex               :: S.Set BlockHash
    ,   nodeDistance            :: Double -- think
  }
  deriving (Show, Eq, Ord)


data Neighbor = Neighbor {
        neighborPosition    :: NodePosition
    ,   neighborId          :: NodeId
  }
  deriving (Show, Eq, Ord)


data ShardingNodeAction =
    -- TODO think requestBlockIndex requestNeededBlocks, find position
    ---    InitAction
        NewNodeInNetAction          NodeId NodePosition
    -- TODO create index for new node by NodeId
    |   BlockIndexCreateAction      NodeId
    |   BlockIndexAcceptAction      [BlockHash]
    |   BlocksAcceptAction          [(BlockHash, Block)]
    ---
    |   CleanBlocksAction -- clean local blocks
    --- ShiftAction => NewPosiotionResponse
    |   ShiftAction
    |   TheNodeHaveNewCoordinates   NodeId NodePosition
    ---- NeighborListRequest => NeighborListAcceptAction
    |   NeighborListAcceptAction   [(NodeId, NodePosition)]
    |   TheNodeIsDead               NodeId


data ShardingNodeRequestAndResponce =
        IamAwakeRequst        MyNodeId MyNodePosition -- broadcast for all network
    ----
    |   BlockIndexRequest     [NodeId]    -- for neighbors
    |   BlockIndexResponse    NodeId [BlockHash]
    |   BlockListRequest      [BlockHash]
    |   BlockListResponse     NodeId [(BlockHash, Block)]
    --- ShiftAction => NewPosiotionResponse
    |   NewPosiotionResponse   MyNodePosition
    ---
    |   NeighborListRequest -- ask net level new neighbors
  deriving (Show)
--
hashToPoint :: BlockHash -> Point
hashToPoint (x1, x2, _, _, _, _, _, _) = Point x1 x2


makeEmptyShardingNode :: S.Set Neighbor ->  MyNodeId -> MyNodePosition -> S.Set BlockHash -> ShardingNode
makeEmptyShardingNode aNeighbors aMyNodeId aMyPosition aMyBlockIndex = ShardingNode {
        nodeNeighbors   = aNeighbors
    ,   shardingNodeId  = aMyNodeId
    ,   nodePosition    = aMyPosition
    ,   nodeIndex       = aMyBlockIndex
    ,   nodeDistance    = 1
  }

makeEmptyNeighbor :: NodePosition -> NodeId -> Neighbor
makeEmptyNeighbor aPosition aNodeId = Neighbor {
        neighborPosition    = aPosition
    ,   neighborId          = aNodeId
  }
