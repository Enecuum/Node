module Sharding.Space.Shift where

import              Sharding.Space.Point
import              Sharding.Space.Distance

import              Data.List.Extra
import              Data.Word
import qualified    Data.Set            as S
import              Service.InfoMsg
import              Control.Concurrent.Chan
import              Node.Data.GlobalLoging


checkUnevenness :: MyNodePosition -> S.Set NodePosition -> Bool
checkUnevenness aMyNodePosition aPositions =
  -- TODO CHECK (length aDistances == 4)
    (length aDistances == 4) && (minimum aDistances `div` 4 < maximum aDistances `div` 5)
  where
    aDistances = distanceTo aMyNodePosition <$>
        findNearestNeighborPositions aMyNodePosition aPositions

findNearestNeighborPositions :: MyNodePosition -> S.Set NodePosition -> [NodePosition]
findNearestNeighborPositions aMyNodePosition aPositions =
    head . sortOn (distanceTo aMyNodePosition) <$> aFilteredPositions
  where
    aFilteredPositions :: [[NodePosition]]
    aFilteredPositions = S.toList <$> filter (not . S.null) aSeparatedPositions

    aSeparatedPositions :: [S.Set NodePosition]
    aSeparatedPositions = aFilter <$> findSupportPoints aMyNodePositionPoint
      where
        MyNodePosition aMyNodePositionPoint = aMyNodePosition

    aFilter :: Point -> S.Set NodePosition
    aFilter aSupportPoint = S.filter
        (\aPosition -> distanceTo (NodePosition aSupportPoint) aPosition < maxBound`div`2)
        aPositions



shiftToCenterOfMass :: MyNodePosition -> S.Set NodePosition -> MyNodePosition
shiftToCenterOfMass aMyNodePosition aNearestPositions = do
    MyNodePosition (Point aX1 aX2)
  where
    aX1 = aFoonc (halfOfMaxBound `minusInWorld` x1) xh1 xh2
    aX2 = aFoonc (halfOfMaxBound `minusInWorld` x2) yh1 yh2

    --aFoonc aDiff ah1 ah2 = fromInteger
    --    ((toInteger (aDiff `plusInWorld` ah1) `plusInWorld`
    --      toInteger (aDiff `plusInWorld` ah2))`div`2) `minusInWorld` aDiff

    aFoonc aDiff ah1 ah2 = ((aDiff `plusInWorld` ah1) `plusInWorld` ((aDiff `plusInWorld` ah2) `div` 2)) `minusInWorld` aDiff


    NodePosition (Point xh1 _) = aFind (Point (x1 `plusInWorld` maxBound`div`4) x2) distX1
    NodePosition (Point xh2 _) = aFind (Point (x1 `minusInWorld` maxBound`div`4) x2) distX1
    NodePosition (Point _ yh1) = aFind (Point x1 (x2 `plusInWorld` maxBound`div`4)) distX2
    NodePosition (Point _ yh2) = aFind (Point x1 (x2 `minusInWorld` maxBound`div`4)) distX2

    aFind :: Point -> (Point -> Point -> Word64) -> NodePosition
    aFind aPositionPoint = findSupportNeighborPosition
        aMyNodePosition aNearestPositions (NodePosition aPositionPoint)

    MyNodePosition (Point x1 x2) = aMyNodePosition


findSupportNeighborPosition ::
        MyNodePosition
    ->  S.Set NodePosition
    ->  NodePosition
    -> (Point -> Point -> Word64)
    ->  NodePosition
findSupportNeighborPosition aMyNodePosition aNearestPositions aSupportPoint aDist =
    head $ sortOn aDistanceToMe $
    filter aIsInSupportFourthOfSpace $ S.elems aNearestPositions
  where
    aIsInSupportFourthOfSpace :: NodePosition -> Bool
    aIsInSupportFourthOfSpace aNodePosition =
        distanceTo aSupportPoint aNodePosition < maxBound `div` 2

    aDistanceToMe :: NodePosition -> Distance Point
    aDistanceToMe (NodePosition aNeighborPosition) =
        aDist aMyNodePositionPoint aNeighborPosition

    MyNodePosition aMyNodePositionPoint = aMyNodePosition
--
