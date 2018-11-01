module Enecuum.Research.RouteDrewing where

import           Universum
import           Data.HGraph.StringHashable
import           Graphics.GD.Extra

hashToPhase :: D.StringHash -> Double
hashToPhase hash = (fromIntegral (integerToHash hash) - qoh / 2) / qoh * pi
    where
        qoh = fromIntegral quantityOfHashes