{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Sharding.Space.Point where

import              Data.Serialize
import              Data.Word
import              GHC.Generics

-- * Points in the testing space
data Point = Point !Word64 !Word64
    deriving (Eq, Ord, Show, Generic)

instance Serialize Point

newtype MyNodePosition  = MyNodePosition Point deriving (Eq, Ord, Show, Serialize)
newtype NodePosition    = NodePosition   Point deriving (Eq, Ord, Show, Serialize)
newtype ShardPosition   = ShardPosition  Point deriving (Eq, Ord, Show, Serialize)
newtype PointFrom       = PointFrom      Point deriving (Eq, Ord, Show, Serialize)
newtype PointTo         = PointTo        Point deriving (Eq, Ord, Show, Serialize)

class NodePositions a b where
    toNodePosition :: a -> b

instance (Positions a, Positions b) => NodePositions a b where
    toNodePosition = fromPoint.toPoint

class Positions points where
    toPoint :: points -> Point
    fromPoint :: Point -> points


instance Positions MyNodePosition where
    toPoint (MyNodePosition p) = p
    fromPoint = MyNodePosition


instance Positions NodePosition where
    toPoint (NodePosition p) = p
    fromPoint = NodePosition


instance Positions ShardPosition where
    toPoint (ShardPosition p) = p
    fromPoint = ShardPosition


instance Positions PointFrom where
    toPoint (PointFrom p) = p
    fromPoint = PointFrom


instance Positions PointTo where
    toPoint (PointTo p) = p
    fromPoint = PointTo

-- | Find the support points.
{-# INLINE findSupportPoints #-}
findSupportPoints :: Point -> [Point]
findSupportPoints (Point x1 x2) = [
    Point (x1 + fourthOfMaxBound) x2,
    Point (x1 - fourthOfMaxBound) x2,
    Point x1 (x2 + fourthOfMaxBound),
    Point x1 (x2 - fourthOfMaxBound)]


halfOfMaxBound :: (Bounded num, Num num, Integral num) => num
halfOfMaxBound = maxBound `div` 2


fourthOfMaxBound :: (Bounded num, Num num, Integral num) => num
fourthOfMaxBound = maxBound `div` 4
