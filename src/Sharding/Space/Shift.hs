module Sharding.Space.Shift where

import              Sharding.Space.Point
import              Sharding.Space.Distance

import              Data.List.Extra
import              Data.Word
import qualified    Data.Set            as S


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
        (\aPosition -> distanceTo (NodePosition aSupportPoint) aPosition < fourthOfMaxBound)
        aPositions



shiftToCenterOfMass :: MyNodePosition -> S.Set NodePosition -> MyNodePosition
shiftToCenterOfMass aMyNodePosition aNearestPositions =
    MyNodePosition $ Point aX1 aX2
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
    aFind aPositionPoint = findSuportNeighborPosition
        aMyNodePosition aNearestPositions (NodePosition aPositionPoint)

    MyNodePosition (Point x1 x2) = aMyNodePosition


findSuportNeighborPosition ::
        MyNodePosition
    ->  S.Set NodePosition
    ->  NodePosition
    -> (Point -> Point -> Word64)
    ->  NodePosition
findSuportNeighborPosition aMyNodePosition aNearestPositions aSupportPoint aDist =
    head $ sortOn aDistanceToMe $
    filter aIsInSupportFourthOfSpace $ S.elems aNearestPositions
  where
    aIsInSupportFourthOfSpace :: NodePosition -> Bool
    aIsInSupportFourthOfSpace aNodePosition =
        distanceTo aSupportPoint aNodePosition < fourthOfMaxBound

    aDistanceToMe :: NodePosition -> Distance Point
    aDistanceToMe (NodePosition aNeighborPosition) =
        aDist aMyNodePositionPoint aNeighborPosition

    MyNodePosition aMyNodePositionPoint = aMyNodePosition
--
