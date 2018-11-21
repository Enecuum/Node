module Enecuum.Research.RouteDrawing where

import           Universum
import           Data.HGraph.StringHashable
import           Enecuum.Research.ChordRouteMap

hashToPhase :: StringHash -> Double
hashToPhase hash = (fromIntegral (hashToWord64 hash) - fromIntegral (maxBound :: Word64) / 2) / (2^(keySize - 1)) * pi
