{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module Sharding.Sharding where

import              Sharding.Space.Distance
import              Sharding.Space.Points
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

--------------------------------------------------------------------------------


findNodeDomain :: MyNodePosition -> S.Set NodePosition -> Distance Point
findNodeDomain aMyPosition aPositions = if
    | length aNearestPoints < 4 -> maxBound
    | otherwise                 ->
        last . sort $ distanceTo aMyPosition <$> aNearestPoints
  where
    aNearestPoints = findNearestNeighborPositions aMyPosition aPositions


findNearestNeighborPositions :: MyNodePosition -> S.Set NodePosition -> [NodePosition]
findNearestNeighborPositions aMyNodePosition aPositions = head <$>
     sortOn (distanceTo aMyNodePosition) <$> aFilteredPositions
  where
    aFilteredPositions :: [[NodePosition]]
    aFilteredPositions = S.toList <$> filter (not . S.null) aSeparatedPositions

    aSeparatedPositions :: [S.Set NodePosition]
    aSeparatedPositions = aFilter <$> findSupportPoints aMyNodePositionPoint
      where
        MyNodePosition aMyNodePositionPoint = aMyNodePosition

    aFilter :: Point -> S.Set NodePosition
    aFilter aSupportPoint = S.filter
        (\aPosition -> distanceTo (NodePosition aSupportPoint) aPosition < fourthOfMaxBound)
        aPositions

checkUnevenness :: MyNodePosition -> S.Set NodePosition -> Bool
checkUnevenness aMyNodePosition aPositions =
    minimum aDistances `div` 4 < maximum aDistances `div` 5
  where
    aDistances = distanceTo aMyNodePosition <$>
        findNearestNeighborPositions aMyNodePosition aPositions


shiftToCenterOfMass :: ShardingNode -> MyNodePosition
shiftToCenterOfMass aShardingNode = MyNodePosition $ Point aX1 aX2
  where
    aX1 = aFoonc (halfOfMaxBound - x1) xh1 xh2
    aX2 = aFoonc (halfOfMaxBound - x2) yh1 yh2

    aFoonc aDiff ah1 ah2 = fromInteger
        ((toInteger (aDiff + ah1) +
          toInteger (aDiff + ah2))`div`2) - aDiff

    NodePosition (Point xh1 _) = aFind (Point (x1 + fourthOfMaxBound) x2) distX1
    NodePosition (Point xh2 _) = aFind (Point (x1 - fourthOfMaxBound) x2) distX1
    NodePosition (Point _ yh1) = aFind (Point x1 (x2 + fourthOfMaxBound)) distX2
    NodePosition (Point _ yh2) = aFind (Point x1 (x2 - fourthOfMaxBound)) distX2

    aFind :: Point -> (Point -> Point -> Word64) -> NodePosition
    aFind a b = neighborPosition $ findSuportNeighbor aShardingNode a b

    MyNodePosition (Point x1 x2) = nodePosition aShardingNode


findSuportNeighbor ::
        ShardingNode
    ->  Point
    -> (Point -> Point -> Word64)
    ->  Neighbor
findSuportNeighbor aShardingNode aSupportPoint aDist =
    head $ sortOn aDistanceToMe $
    filter (\aNeighbor -> aDistanceTo aNeighbor < fourthOfMaxBound) aNeighbors
  where
    MyNodePosition aMyPosition = nodePosition aShardingNode

    aNeighbors :: [Neighbor]
    aNeighbors = S.elems $ nodeNeighbors aShardingNode

    aDistanceTo :: Neighbor -> Distance Point
    aDistanceTo (neighborPosition -> NodePosition aNeighborPosition) =
        rhombusDistance aNeighborPosition aSupportPoint

    aDistanceToMe :: Neighbor -> Distance Point
    aDistanceToMe (neighborPosition -> NodePosition aNeighborPosition) =
        aDist aMyPosition aNeighborPosition
