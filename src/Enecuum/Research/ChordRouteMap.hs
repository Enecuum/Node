module Enecuum.Research.ChordRouteMap
    ( ChordRouteMap
    , addToMap
    , removeFromMap
    , findInMap
    , findInMapR
    , findInMapNByKey
    , findNext
    , hashSize
    , quantityOfHashes
    , toChordRouteMap
    ) where

import           Universum
import qualified Data.Map                      as M
import           Data.HGraph.StringHashable

-- | Route map for chord algorithm.
type ChordRouteMap a = M.Map Integer a

toChordRouteMap :: Ord a => [(StringHash, a)] -> ChordRouteMap a
toChordRouteMap s = M.fromList [(hashToInteger k, v)|(k, v) <- s]

-- | Size of hashes.
hashSize :: Integer
hashSize = 256

-- | Quantity of hashes.
quantityOfHashes :: Integer
quantityOfHashes = 2 ^ hashSize

-- | Add elem to route map.
addToMap :: Ord a => StringHash -> a -> ChordRouteMap a -> ChordRouteMap a
addToMap hash = M.insert (hashToInteger hash)

-- | Remove elem from route map. 
removeFromMap :: Ord a => StringHash -> ChordRouteMap a -> ChordRouteMap a
removeFromMap hash = M.delete (hashToInteger hash)

-- | Find all fingers in rout map by straight formulas.
findInMap :: Ord a => StringHash -> ChordRouteMap a -> [(StringHash, a)]
findInMap = findInMapByKey
    (\hash i -> (hashToInteger hash + 2 ^ i) `mod` quantityOfHashes)

-- | Find all fingers in rout map by inverse formulas.
findInMapR :: Ord a => StringHash -> ChordRouteMap a -> [(StringHash, a)]
findInMapR = findInMapByKey
    (\hash i -> (quantityOfHashes + hashToInteger hash - 2 ^ i) `mod` quantityOfHashes)

-- | Find all fingers in rout map by formulas.
findInMapByKey
    :: Ord a
    => (StringHash -> Integer -> Integer)
    -> StringHash
    -> ChordRouteMap a
    -> [(StringHash, a)]
findInMapByKey elemKey hash rm = mapMaybe
    (\i -> findInMapNByKey elemKey i hash rm) [0..hashSize-1]

-- | Find N finger in rout map by formulas.
findInMapNByKey
    :: (StringHash -> Integer -> Integer)
    -> Integer
    -> StringHash
    -> Map Integer b
    -> Maybe (StringHash, b)
findInMapNByKey elemKey i hash rm = 
    (\(x, y) -> (integerToHash x, y)) <$>
    (if isJust bottomElem then bottomElem else topElem)
    where
        topElem    = M.lookupLE quantityOfHashes rm
        bottomElem = M.lookupLE (elemKey hash i) rm

-- | Find the closest elem to hash from rout map.
findNext :: Ord a => StringHash -> ChordRouteMap a -> Maybe a
findNext hash rm = if isJust bottomElem then bottomElem else topElem
    where
        bottomElem = snd <$> M.lookupLE elemKey rm
        topElem    = snd <$> M.lookupLE quantityOfHashes rm
        elemKey    = hashToInteger hash