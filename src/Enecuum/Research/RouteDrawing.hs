module Enecuum.Research.RouteDrawing where

import           Universum
import           Data.HGraph.StringHashable
import           Enecuum.Research.ChordRouteMap
import           Graphics.GD.Extra

hashToPhase :: StringHash -> Double
hashToPhase hash = (fromIntegral (hashToInteger hash) - qoh / 2) / qoh * pi
    where
        qoh = fromIntegral quantityOfHashes