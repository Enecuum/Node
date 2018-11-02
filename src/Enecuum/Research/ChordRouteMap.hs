module Enecuum.Research.ChordRouteMap
    ( ChordRouteMap
    , addToMap
    , removeFromMap
    , findInMap
    , findInMapNByKey
    , findNextResender
    , hashSize
    , quantityOfHashes
    , toChordRouteMap
    , findNextForHash
    , findPreviusForHash
    , fromChordRouteMap
    ) where

import           Universum
import qualified Data.Map                      as M
import           Data.HGraph.StringHashable

-- | Route map for chord algorithm.
type ChordRouteMap a = M.Map Word64 (StringHash, a)

toChordRouteMap :: Ord a => [(StringHash, a)] -> ChordRouteMap a
toChordRouteMap s = M.fromList [(hashToWord64 k, (k, v))|(k, v) <- s]

fromChordRouteMap :: ChordRouteMap a -> [(StringHash, a)]
fromChordRouteMap = M.elems

-- | Size of hashes.
hashSize :: Integer
hashSize = 256

-- | Quantity of hashes.
quantityOfHashes :: Integer
quantityOfHashes = 2 ^ hashSize

-- | Add elem to route map.
addToMap :: Ord a => StringHash -> a -> ChordRouteMap a -> ChordRouteMap a
addToMap hash e = M.insert (hashToWord64 hash) (hash, e)

-- | Remove elem from route map.
removeFromMap :: Ord a => StringHash -> ChordRouteMap a -> ChordRouteMap a
removeFromMap hash = M.delete (hashToWord64 hash)

-- | Find all fingers in rout map by straight formula.
findInMap :: Ord a => StringHash -> ChordRouteMap a -> [(StringHash, a)]
findInMap = findInMapByKey (\hash i -> hashToWord64 hash + 2 ^ i)


-- | Find all fingers in route map by formula.
--  counterclockwise direction
findInMapByKey
    :: Ord a
    => (StringHash -> Word64 -> Word64)
    -> StringHash
    -> ChordRouteMap a
    -> [(StringHash, a)]
findInMapByKey elemKey hash rm = mapMaybe
    (\i -> findInMapNByKey elemKey i hash rm) [0..63]

-- | Find N finger in route map by formula.
--  counterclockwise direction
findInMapNByKey
    :: (StringHash -> Word64 -> Word64)
    -> Word64
    -> StringHash
    -> ChordRouteMap b
    -> Maybe (StringHash, b)
findInMapNByKey elemKey i hash rm = snd <$>
    (if isJust bottomElem then bottomElem else topElem)
    where
        topElem    = M.lookupLE maxBound rm
        bottomElem = M.lookupLE (elemKey hash i) rm

--  clockwise direction
findPreviusForHash :: StringHash -> ChordRouteMap b -> Maybe (StringHash, b)
findPreviusForHash = findInMapNByKey (\hash i -> hashToWord64 hash - 2 ^ i) 0

--  clockwise direction
findNextForHash :: StringHash -> ChordRouteMap b -> Maybe (StringHash, b)
findNextForHash hash rm = snd <$> (if isJust topElem then topElem else bottomElem)
    where
        bottomElem = M.lookupGE 0 rm
        topElem    = M.lookupGE (hashToWord64 hash + 1) rm

-- | Find the closest elem to hash from route map.
findNextResender :: Ord a => StringHash -> ChordRouteMap a -> Maybe (StringHash, a)
findNextResender hash rm = if isJust bottomElem then bottomElem else topElem
    where
        bottomElem = snd <$> M.lookupLE elemKey  rm
        topElem    = snd <$> M.lookupLE maxBound rm
        elemKey    = hashToWord64 hash
