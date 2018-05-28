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
        (\aPosition -> distanceTo (NodePosition aSupportPoint) aPosition < maxBound`div`2)
        aPositions



shiftToCenterOfMass :: MyNodePosition -> S.Set NodePosition -> MyNodePosition
shiftToCenterOfMass aMyNodePosition aNearestPositions
      | S.size aNearestPositions == 0 = aMyNodePosition
      | S.size aNearestPositions == 1 = moveToOneSixteenth aMyNodePosition (head $ S.elems aNearestPositions)
      | S.size aNearestPositions >= 2 = moveToTriangle aMyNodePosition $ take 2 $ sortOn (\a -> distanceTo aMyNodePosition a) $ S.toList aNearestPositions


moveToOneSixteenth :: MyNodePosition -> NodePosition -> MyNodePosition
moveToOneSixteenth (MyNodePosition (Point x1 y1)) (NodePosition (Point x2 y2))
      | x1 <= x2 && y1 <= y2 = MyNodePosition $ Point (x1 `minusInWorld` oneSixteenth) (y1 `minusInWorld` oneSixteenth)
      | x1 > x2 && y1 <= y2  = MyNodePosition $ Point (x1 `plusInWorld` oneSixteenth) (y1 `minusInWorld` oneSixteenth)
      | x1 <= x2 && y1 > y2  = MyNodePosition $ Point (x1 `minusInWorld` oneSixteenth) (y1 `plusInWorld` oneSixteenth)
      | otherwise            = MyNodePosition $ Point (x1 `plusInWorld` oneSixteenth) (y1 `plusInWorld` oneSixteenth)

moveToTriangle :: MyNodePosition -> [NodePosition] -> MyNodePosition
moveToTriangle aMyNodePosition p1@((NodePosition (Point x1 y1)) : p2@(NodePosition (Point x2 y2) :[])) =
    --if (distanceTo p1 p2) < ((distanceTo aMyNodePosition p1))
    if distanceTo aMyNodePosition (NodePosition $ Point xm1 ym1) > distanceTo aMyNodePosition (NodePosition $ Point xm2 ym2)
    then MyNodePosition (Point xm2 ym2)
    else MyNodePosition (Point xm1 ym1)
    where
      mx = x2 `minusInWorld` x1
      my = y2 `minusInWorld` y1
      xm1 = ((div mx 2) `minusInWorld` (div (my*866) 1000) ) `plusInWorld` x1
      xm2 = ((div mx 2) `plusInWorld`  (div (my*866) 1000) ) `plusInWorld` x1
      ym1 = ((div my 2) `plusInWorld`  (div (mx*866) 1000) ) `plusInWorld` x1
      ym2 = ((div my 2) `minusInWorld` (div (mx*866) 1000) ) `plusInWorld` x1
moveToTriangle a _ = a


oneSixteenth :: (Bounded num, Num num, Integral num) => num
oneSixteenth = maxBound `div` 16

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
