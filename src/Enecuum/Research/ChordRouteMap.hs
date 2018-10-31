module Enecuum.Research.ChordRouteMap
    ( ChordRouteMap
    , addToMap
    , removeFromMap
    , findInMap
    , findInMapR
    , findNext
    ) where

import           Universum
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.HGraph.StringHashable

type ChordRouteMap a = M.Map Integer a

hashSize :: Integer
hashSize = 256

elemNumber :: Integer
elemNumber = 2 ^ hashSize

addToMap :: Ord a => StringHash -> a -> ChordRouteMap a -> ChordRouteMap a
addToMap hash = M.insert (hashToInteger hash)

removeFromMap :: Ord a => StringHash -> ChordRouteMap a -> ChordRouteMap a
removeFromMap hash = M.delete (hashToInteger hash)

findInMap :: Ord a => StringHash -> ChordRouteMap a -> Set (StringHash, a)
findInMap = findInMapByKey
    (\hash i -> (hashToInteger hash + 2 ^ i) `mod` elemNumber)

findInMapR :: Ord a => StringHash -> ChordRouteMap a -> Set (StringHash, a)
findInMapR = findInMapByKey
    (\hash i -> (elemNumber + hashToInteger hash - 2 ^ i) `mod` elemNumber)

findInMapByKey
    :: Ord a
    => (StringHash -> Integer -> Integer)
    -> StringHash
    -> ChordRouteMap a
    -> Set (StringHash, a)
findInMapByKey elemKey hash rm = S.fromList $ mapMaybe
    (\i -> findInMapByKeyN elemKey i hash rm) [0..hashSize-1]

findInMapByKeyN
    :: (StringHash -> Integer -> Integer)
    -> Integer
    -> StringHash
    -> Map Integer b
    -> Maybe (StringHash, b)
findInMapByKeyN elemKey i hash rm = 
    (\(x, y) -> (integerToHash x, y)) <$>
    (if isJust bottomElem then bottomElem else topElem)
    where
        topElem    = M.lookupLE elemNumber rm
        bottomElem = M.lookupLE (elemKey hash i) rm

findNext :: Ord a => StringHash -> ChordRouteMap a -> Maybe a
findNext hash rm = if isJust bottomElem then bottomElem else topElem
    where
        bottomElem = snd <$> M.lookupLE elemKey rm
        topElem    = snd <$> M.lookupLE elemNumber rm
        elemKey    = hashToInteger hash