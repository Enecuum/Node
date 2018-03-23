module Sharding.Types where

import              Node.Data.Data

import              Sharding.Space.Points
import              Sharding.Space.Distance

import              Data.Word
import qualified    Data.ByteString     as B
import qualified    Data.Set            as S

type BlockHash = (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64)
newtype MyPosition = MyPosition Point deriving (Eq, Ord, Show)
newtype Position   = Position Point deriving (Eq, Ord, Show)

data Block = Block BlockHash B.ByteString deriving (Ord, Eq, Show)

data ShardingNode = ShardingNode {
        nodeNeighbors           :: S.Set Neighbor
    ,   shardingNodeId          :: MyNodeId
    ,   nodePosition            :: MyPosition
    ,   nodeIndex               :: S.Set BlockHash
    ,   nodeDistance            :: Double -- think
  }
  deriving (Show, Eq, Ord)


data Neighbor = Neighbor {
        neighborDistance    :: Distance Point
    ,   neighborPosition    :: Position
    ,   neighborId          :: NodeId
  }
  deriving (Show, Eq, Ord)


data ShardingNodeAction =
    -- TODO think requestBlockIndex requestNeededBlocks, find position
        InitAction
    |   NewNodeInNetAction          NodeId Point
    -- TODO create index for new node by NodeId
    |   BlockIndexCreateAction      NodeId
    |   BlockIndexAcceptAction      [BlockHash]
    |   BlocksAcceptAction          [(BlockHash, Block)]
    ---
    |   CleanBlocksAction -- clean local blocks
    --- ShiftAction => NewPosiotionResponse
    |   ShiftAction
    |   TheNodeHaveNewCoordinates   NodeId Position
    ---- NeighborListRequest => NeighborListAcceptAction
    |   NeighborListAcceptAction   [(NodeId, Position)]
    |   TheNodeIsDead               NodeId


data ShardingNodeRequestAndResponce =
        IamAwakeRequst        NodeId Point -- broadcast for all network
    ----
    |   BlockIndexRequest     [NodeId]    -- for neighbors
    |   BlockIndexResponse    NodeId [BlockHash]
    |   BlockListRequest      [BlockHash]
    |   BlockListResponse     NodeId [(BlockHash, Block)]
    --- ShiftAction => NewPosiotionResponse
    |   NewPosiotionResponse   MyPosition
    ---
    |   NeighborListRequest -- ask net level new neighbors

--
hashToPoint :: BlockHash -> Point
hashToPoint (x1, x2, _, _, _, _, _, _) = Point x1 x2
