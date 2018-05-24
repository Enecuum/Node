{-# LANGUAGE
        DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   FlexibleInstances
    ,   MultiParamTypeClasses
  #-}

module Sharding.Space.Point where

import              Data.Serialize
import              Data.Word
import              Node.Data.Key
import              GHC.Generics

-- * Points in the testing space
data Point = Point !Word64 !Word64
    deriving (Eq, Ord, Show, Read, Generic)

instance Serialize Point

newtype MyNodePosition  = MyNodePosition Point
    deriving (Eq, Ord, Show, Read,Positions, Serialize)

newtype NodePosition    = NodePosition   Point
    deriving (Eq, Ord, Show, Read,Positions, Serialize)

newtype ShardPosition   = ShardPosition  Point
    deriving (Eq, Ord, Show, Read, Positions, Serialize)

newtype PointFrom       = PointFrom      Point
    deriving (Eq, Ord, Show, Read, Positions, Serialize)

newtype PointTo         = PointTo        Point
    deriving (Eq, Ord, Show, Read, Positions, Serialize)


class NodePositions a b where
    toNodePosition :: a -> b


idToPoint :: NodeId -> Point
idToPoint (NodeId aId) = Point x y
  where
    x        = fromInteger $ aId `mod` aMaxWord
    y        = fromInteger $ aId `div` aMaxWord `mod` aMaxWord
    aMaxWord = toInteger  (maxBound :: Word64)


instance (Positions a, Positions b) => NodePositions a b where
    toNodePosition = fromPoint.toPoint


class Positions points where
    toPoint   :: points -> Point
    fromPoint :: Point -> points

instance Positions Point where
    toPoint  = id
    fromPoint = id


-- | Find the support points.
{-# INLINE findSupportPoints #-}
findSupportPoints :: Point -> [Point]
findSupportPoints (Point x1 x2) = [
    Point (x1 `plusInWorld` fourthOfMaxBound) x2,
    Point (x1 `minusInWorld` fourthOfMaxBound) x2,
    Point x1 (x2 `plusInWorld` fourthOfMaxBound),
    Point x1 (x2 `minusInWorld` fourthOfMaxBound)]

plusInWorld :: Word64 -> Word64 -> Word64
plusInWorld a b = minusInWorld a (maxBound - b)

--
minusInWorld :: Word64 -> Word64 -> Word64
minusInWorld a b | a >= b = a - b
                 | otherwise = maxBound - (b-a)

halfOfMaxBound :: (Bounded num, Num num, Integral num) => num
halfOfMaxBound = maxBound `div` 2


fourthOfMaxBound :: (Bounded num, Num num, Integral num) => num
fourthOfMaxBound = maxBound `div` 4
