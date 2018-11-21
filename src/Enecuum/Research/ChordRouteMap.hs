module Enecuum.Research.ChordRouteMap
    ( ChordRouteMap
    , addToMap
    , removeFromMap
    , getByHash
    , findInMap
    , findInMapNByKey
    , findNextResender
    , hashSize
    , keySize
    , quantityOfHashes
    , toChordRouteMap
    , findNextForHash
    , findPreviousForHash
    , fromChordRouteMap
    , nextForHello
    , findConnectByHash
    ) where

import           Universum
import qualified Data.Map                      as M
import qualified Data.List                     as List
import           Data.HGraph.StringHashable

-- | Route map for chord algorithm.
type ChordRouteMap a = M.Map Word64 (StringHash, a)

toChordRouteMap :: Ord a => [(StringHash, a)] -> ChordRouteMap a
toChordRouteMap s = M.fromList [(hashToWord64 k, (k, v))|(k, v) <- s]

fromChordRouteMap :: ChordRouteMap a -> [(StringHash, a)]
fromChordRouteMap = M.elems

getByHash :: Ord a => StringHash -> ChordRouteMap a -> Maybe a
getByHash hash routeMap = snd <$> hashToWord64 hash `M.lookup` routeMap

-- | Size of hashes.
hashSize :: Integer
hashSize = 256

keySize :: Word64
keySize = 64

-- | Quantity of hashes.
quantityOfHashes :: Integer
quantityOfHashes = 2 ^ hashSize

-- | Add elem to route map.
addToMap :: Ord a => StringHash -> a -> ChordRouteMap a -> ChordRouteMap a
addToMap hash e = M.insert (hashToWord64 hash) (hash, e)

-- | Remove elem from route map.
removeFromMap :: Ord a => StringHash -> ChordRouteMap a -> ChordRouteMap a
removeFromMap hash = M.delete (hashToWord64 hash)

findConnectByHash :: Ord a => StringHash -> ChordRouteMap a -> Maybe a
findConnectByHash hash rm = snd <$> M.lookup (hashToWord64 hash) rm

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
    (\i -> findInMapNByKey elemKey i hash rm) [0..keySize - 1]

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
findPreviousForHash :: StringHash -> ChordRouteMap b -> Maybe (StringHash, b)
findPreviousForHash = findInMapNByKey (\hash i -> hashToWord64 hash - 2 ^ i) 0

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

nextForHello :: Ord a => StringHash -> StringHash -> ChordRouteMap a -> Maybe a
nextForHello myHash senderHash rm = case findNextResender senderHash rm of
    Just (nextHash, address)
        | hashCompare [myHash, nextHash, senderHash] -> pure address
        | hashCompare [nextHash, senderHash, myHash] -> pure address
        | hashCompare [senderHash, myHash, nextHash] -> pure address
    _ -> Nothing
    where
        (.>.) :: StringHash -> StringHash -> Bool
        h1 .>. h2 = hashToWord64 h1 > hashToWord64 h2

        hashCompare :: [StringHash] -> Bool
        hashCompare = (== 1) . List.length . List.groupBy (.>.)
