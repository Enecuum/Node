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
      | S.size aNearestPositions == 2 = moveToTriangle aMyNodePosition $ S.toList aNearestPositions
      | S.size aNearestPositions > 3  = moveToSquere aMyNodePosition $ take 3 $ sortOn (\a -> distanceTo aMyNodePosition a) $ S.toList aNearestPositions



moveToOneSixteenth :: MyNodePosition -> NodePosition -> MyNodePosition
moveToOneSixteenth (MyNodePosition (Point x1 y1)) (NodePosition (Point x2 y2))
      | x1 <= x2 && y1 <= y2 = MyNodePosition $ Point (x1 `minusInWorld` oneSixteenth) (y1 `minusInWorld` oneSixteenth)
      | x1 > x2 && y1 <= y2  = MyNodePosition $ Point (x1 `plusInWorld` oneSixteenth) (y1 `minusInWorld` oneSixteenth)
      | x1 <= x2 && y1 > y2  = MyNodePosition $ Point (x1 `minusInWorld` oneSixteenth) (y1 `plusInWorld` oneSixteenth)
      | otherwise            = MyNodePosition $ Point (x1 `plusInWorld` oneSixteenth) (y1 `plusInWorld` oneSixteenth)

moveToTriangle :: MyNodePosition -> [NodePosition] -> MyNodePosition
moveToTriangle aMyNodePosition ((NodePosition (Point x1 y1)) : (NodePosition (Point x2 y2) :[])) =
    if distanceTo aMyNodePosition (NodePosition $ Point xm2 ym2) > distanceTo aMyNodePosition (NodePosition $ Point xm3 ym3)
    then MyNodePosition (Point xm3 ym3)
    else MyNodePosition (Point xm2 ym2)
    where
      xm = (x1 `plusInWorld` x2) `div` 2
      ym = (y1 `plusInWorld` y2) `div` 2
      k1 ::Double
      k1 = ((fromIntegral x1) - (fromIntegral x2)) / ((fromIntegral y1) - (fromIntegral y2))
      k2 = - 1/k1
      k3 = 1/k1
      b :: Word64
      b = (round $ ((fromIntegral ym) - k2 * (fromIntegral xm))) ::Word64

      xdiff = (fromIntegral x1) - (fromIntegral x2)
      ydiff = (fromIntegral y1) - (fromIntegral y2)
      lenLine :: Double
      lenLine = xdiff**2 + ydiff **2
      len = if (lenLine < 18446744073709541115/16)
            then lenLine * 1.2
            else lenLine

      xm2:: Word64
      xm2 = minusInWorld (round $ sqrt $ len / (1 + k2 ** 2)) xm
      --xm2 = - (fromIntegral xm) +
      xm3:: Word64
      xm3 = minusInWorld (round $ sqrt $ len / (1 + k3 ** 2)) xm
      --xm3 = - (fromIntegral xm) + sqrt $ lenLine / (1 + k3 ** 2)
      ym2::Word64
      ym2 = plusInWorld (round $ (k2 * (fromIntegral xm2))) b
      ym3::Word64
      ym3 = plusInWorld (round $ (k3 * (fromIntegral xm3))) b

moveToTriangle a _ = a

moveToSquere :: MyNodePosition -> [NodePosition] -> MyNodePosition
moveToSquere aMyNodePosition aNodePositions = aMyNodePosition

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
