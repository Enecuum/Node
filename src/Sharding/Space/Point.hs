module Sharding.Space.Point where

import Data.Word

-- * Points in the testing space
data Point = Point !Word64 !Word64
    deriving (Eq, Ord, Show)

newtype MyNodePosition  = MyNodePosition Point deriving (Eq, Ord, Show)
newtype NodePosition    = NodePosition Point deriving (Eq, Ord, Show)
newtype ShardPosition   = ShardPosition Point deriving (Eq, Ord, Show)

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
