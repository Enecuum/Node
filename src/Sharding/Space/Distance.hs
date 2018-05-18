-- | In this module, a distance is defined for space. The space has the form of
--   a multidimensional cube.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts#-}

module Sharding.Space.Distance where

import Data.Word
import Sharding.Space.Point

-- | Point in the testing space.
class Points point where
    type Distance point

    -- | Distance between two points.
    distance        :: point -> point -> Distance point
    rhombusDistance :: point -> point -> Distance point


{-# INLINE dist #-}
dist :: Word64 -> Word64 -> Word64
dist x y = let d = x - y in min d (-d)

distX1, distX2 :: Point -> Point -> Word64
distX1 (Point x1 x2) (Point y1 y2) = dist x1 y1 + dist x2 y2 `div` 2
distX2 (Point x1 x2) (Point y1 y2) = dist x1 y1 `div` 2 + dist x2 y2


instance Points [Word64] where
    type Distance [Word64] = Word64

    {-# INLINE distance #-}
    distance a b = maximum $ zipWith dist a b

    {-# INLINE rhombusDistance #-}
    rhombusDistance a b = sum $ zipWith dist a b

instance Points Point where
    type Distance Point = Word64

    {-# INLINE distance #-}
    distance (Point x1 y1) (Point x2 y2) = max (dist x1 x2) (dist y1 y2)

    {-# INLINE rhombusDistance #-}
    rhombusDistance (Point x1 y1) (Point x2 y2) = dist x1 x2 + dist y1 y2


class DistanceTo a b where
    distanceTo :: a -> b -> Distance Point

instance DistanceTo MyNodePosition PointTo where
    distanceTo (MyNodePosition aMyNodePosition) (PointTo point) =
      rhombusDistance aMyNodePosition point

instance DistanceTo MyNodePosition NodePosition where
    distanceTo (MyNodePosition aMyNodePosition) (NodePosition aNodePosition) =
        rhombusDistance aMyNodePosition aNodePosition


instance DistanceTo NodePosition NodePosition where
    distanceTo (NodePosition x1) (NodePosition x2) = rhombusDistance x1 x2


instance DistanceTo MyNodePosition ShardPosition where
    distanceTo
        (MyNodePosition aMyNodePosition)
        (ShardPosition aShardPosition) =
            distance aMyNodePosition aShardPosition


instance DistanceTo MyNodePosition a => DistanceTo MyNodePosition (Maybe a) where
    distanceTo aPos (Just a) = distanceTo aPos a
    distanceTo _ _ = maxBound

--------------------------------------------------------------------------------
