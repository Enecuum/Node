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


makeEmptyShardingNode :: MyNodeId -> MyPosition -> S.Set BlockHash -> ShardingNode
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
loadInitInformation :: IO (S.Set Neighbor, MyPosition)
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
makeEmptyNeighbor :: Distance Point -> Position -> NodeId -> Neighbor
makeEmptyNeighbor aDistance aPosition aNodeId = Neighbor {
        neighborDistance    = aDistance
    ,   neighborPosition    = aPosition
    ,   neighborId          = aNodeId
  }

--------------------------------------------------------------------------------
findNodeDomain :: ShardingNode -> Distance Point
findNodeDomain aShardingNode = if
    | length aNearestNeighbors < 4 -> maxBound
    | otherwise                    ->
        neighborDistance $ maximum aNearestNeighbors
  where
    aNearestNeighbors = findNearestNeighbors aShardingNode


findNearestNeighbors :: ShardingNode -> [Neighbor]
findNearestNeighbors aShardingNode = S.findMin <$>
    filter (not .S.null) aSeparatedHeighbors
  where
    aSeparatedHeighbors :: [S.Set Neighbor]
    aSeparatedHeighbors = aFilter <$> findSupportPoints aMyPosition

    MyPosition aMyPosition = nodePosition aShardingNode

    aFilter :: Point -> S.Set Neighbor
    aFilter aSupportPoint = S.filter
        (\aNeighbor -> aDistanceToPoint aNeighbor aSupportPoint < fourthOfMaxBound)
        (nodeNeighbors aShardingNode)

    aDistanceToPoint :: Neighbor -> Point -> Distance Point
    aDistanceToPoint aNeighbor aPoint = rhombusDistance aNeighborPosition aPoint
      where
        Position aNeighborPosition = neighborPosition aNeighbor


checkUnevenness :: ShardingNode -> Bool
checkUnevenness aShardingNode =
    minimum aDistances `div` 4 < maximum aDistances `div` 5
  where
    aDistances = neighborDistance <$> findNearestNeighbors aShardingNode


shiftToCenterOfMass :: ShardingNode -> MyPosition
shiftToCenterOfMass aShardingNode = MyPosition $ Point aX1 aX2
  where
    aX1 = aFoonc (halfOfMaxBound - x1) xh1 xh2
    aX2 = aFoonc (halfOfMaxBound - x2) yh1 yh2

    aFoonc aDiff ah1 ah2 = fromInteger
        ((toInteger (aDiff + ah1) +
          toInteger (aDiff + ah2))`div`2) - aDiff

    Position (Point xh1 _) = aFind (Point (x1 + fourthOfMaxBound) x2) distX1
    Position (Point xh2 _) = aFind (Point (x1 - fourthOfMaxBound) x2) distX1
    Position (Point _ yh1) = aFind (Point x1 (x2 + fourthOfMaxBound)) distX2
    Position (Point _ yh2) = aFind (Point x1 (x2 - fourthOfMaxBound)) distX2

    aFind :: Point -> (Point -> Point -> Word64) -> Position
    aFind a b = neighborPosition $ findSuportNeighbor aShardingNode a b

    MyPosition (Point x1 x2) = nodePosition aShardingNode


findSuportNeighbor ::
        ShardingNode
    ->  Point
    -> (Point -> Point -> Word64)
    ->  Neighbor
findSuportNeighbor aShardingNode aSupportPoint aDist =
    head $ sortOn aDistanceToMe $
    filter (\aNeighbor -> aDistanceTo aNeighbor < fourthOfMaxBound) aNeighbors
  where
    MyPosition aMyPosition = nodePosition aShardingNode

    aNeighbors :: [Neighbor]
    aNeighbors = S.elems $ nodeNeighbors aShardingNode

    aDistanceTo :: Neighbor -> Distance Point
    aDistanceTo (neighborPosition -> Position aNeighborPosition) =
        rhombusDistance aNeighborPosition aSupportPoint

    aDistanceToMe :: Neighbor -> Distance Point
    aDistanceToMe (neighborPosition -> Position aNeighborPosition) =
        aDist aMyPosition aNeighborPosition
