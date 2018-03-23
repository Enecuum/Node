{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module Sharding.Sharding where

import              Sharding.Space.Distance
import              Sharding.Space.Points
import              Sharding.Space.Shift
import              Sharding.Types

import              Control.Concurrent.Chan
import              Data.List.Extra
import              Control.Concurrent
import              Control.Monad
import qualified    Data.ByteString     as B
import              Data.Word
import              Node.Data.Data
import              Service.Timer
import qualified    Data.Set            as S



makeEmptyShardingNode :: MyNodeId -> MyNodePosition -> S.Set BlockHash -> ShardingNode
makeEmptyShardingNode aMyNodeId aMyPosition aMyBlockIndex = ShardingNode {
        nodeNeighbors   = S.empty
    ,   shardingNodeId  = aMyNodeId
    ,   nodePosition    = aMyPosition
    ,   nodeIndex       = aMyBlockIndex
  }

-- TODO Is it file or db like sqlite?
loadMyBlockIndex :: IO (S.Set BlockHash)
loadMyBlockIndex = undefined

-- TODO Is it file or db like sqlite?
loadInitInformation :: IO (S.Set Neighbor, MyNodePosition)
loadInitInformation = undefined



--makeShardingNode :: MyNodeId -> Point -> IO ()
makeShardingNode aMyNodeId aMyPoint aChanRequest = do
    aMyBlocksIndex <- loadMyBlockIndex
    (aMyNeighbors, aMyPosition) <- loadInitInformation
    metronome (10^8) $ do
        writeChan aChanRequest CleanBlocksAction
        writeChan aChanRequest ShiftAction

    void $ forkIO $ aLoop
        (makeEmptyShardingNode aMyNodeId aMyPoint aMyBlocksIndex)
  where
    aLoop :: ShardingNode -> IO ()
    aLoop aShardingNode = readChan aChanRequest >>= \case
        _ -> undefined

--------------------------------------------------------------------------------
makeEmptyNeighbor :: Distance Point -> NodePosition -> NodeId -> Neighbor
makeEmptyNeighbor aDistance aPosition aNodeId = Neighbor {
        neighborDistance    = aDistance
    ,   neighborPosition    = aPosition
    ,   neighborId          = aNodeId
  }

--------------------------TODO-TO-REMOVE-------------------------------------------
findNodeDomain :: MyNodePosition -> S.Set NodePosition -> Distance Point
findNodeDomain aMyPosition aPositions = if
    | length aNearestPoints < 4 -> maxBound
    | otherwise                 ->
        last . sort $ distanceTo aMyPosition <$> aNearestPoints
  where
    aNearestPoints = findNearestNeighborPositions aMyPosition aPositions
