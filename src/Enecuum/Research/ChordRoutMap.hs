module Enecuum.Research.ChordRoutMap
    ( ChordRoutMap
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

type ChordRoutMap a = M.Map Integer a

hashSize :: Integer
hashSize = 256

elemNumber :: Integer
elemNumber = 2 ^ hashSize

addToMap :: Ord a => StringHash -> a -> ChordRoutMap a -> ChordRoutMap a
addToMap hash = M.insert (hashToInteger hash)

removeFromMap :: Ord a => StringHash -> ChordRoutMap a -> ChordRoutMap a
removeFromMap hash = M.delete (hashToInteger hash)

findInMap :: Ord a => StringHash -> ChordRoutMap a -> Set (StringHash, a)
findInMap hash rm = S.fromList $ mapMaybe (\i -> findInMapN i hash rm) [0..hashSize-1]

findInMapN :: Ord a => Integer -> StringHash -> ChordRoutMap a -> Maybe (StringHash, a)
findInMapN i hash rm = 
    (\(x, y) -> (integerToHash x, y)) <$>
    (if isJust topElem then topElem else bottomElem)
    where
        topElem    = M.lookupGE elemKey rm
        bottomElem = M.lookupGE 0 rm
        elemKey    = (hashToInteger hash + 2 ^ i) `mod` elemNumber

findInMapR :: Ord a => StringHash -> ChordRoutMap a -> Set (StringHash, a)
findInMapR hash rm = S.fromList $ mapMaybe (\i -> findInMapRN i hash rm) [0..hashSize-1]

findInMapRN :: Ord a => Integer -> StringHash -> ChordRoutMap a -> Maybe (StringHash, a)
findInMapRN i hash rm = 
    (\(x, y) -> (integerToHash x, y)) <$>
    (if isJust bottomElem then bottomElem else topElem)
    where
        bottomElem  = M.lookupLE elemKey rm
        topElem     = M.lookupLE elemNumber rm
        elemKey     = (elemNumber + hashToInteger hash - 2 ^ i) `mod` elemNumber

findNext :: Ord a => StringHash -> ChordRoutMap a -> Maybe a
findNext hash rm = if isJust bottomElem then bottomElem else topElem
    where
        bottomElem = snd <$> M.lookupLE elemKey rm
        topElem    = snd <$> M.lookupLE elemNumber rm
        elemKey    = hashToInteger hash