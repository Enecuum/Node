{-# LANGUAGE TemplateHaskell, DeriveGeneric, ScopedTypeVariables, MultiWayIf #-}

module Sharding.ShardDB.ShardIndex where

import              Control.Exception
import              Lens.Micro.TH
import              Lens.Micro
import              Lens.Micro.Mtl

import              System.Clock
import              Data.Serialize
import qualified    Data.ByteString as B
import              Data.Monoid
import              Data.List.Extra

import qualified    Data.Set as S
import              GHC.Generics

import              Sharding.Types.ShardTypes
import              Sharding.Types.ShardLogic
import              Sharding.Space.Distance
import              Sharding.Space.Point

data ShardExistIndex   = ShardExistIndex {
      _baseSnapshots  :: [SpaceSnapshot]
    , _lastSnapshot   :: SpaceSnapshot
  } deriving (Show, Eq, Ord, Generic)

data ShardNeededIndex  = ShardNeededIndex {
    _setOfHash :: S.Set ShardHash
  }
  deriving (Show, Eq, Ord, Generic)


--  FIXME:  [(ShardHash, Priority, TimeSpec)] => Map ShardHash (Priority, TimeSpec)
data ShardLoadingIndex = ShardLoadingIndex {
    _setOfLoadingShards :: [(ShardHash, Priority, TimeSpec)]
  }
  deriving (Show, Eq, Ord, Generic)

data Priority          = Priority Int
  deriving (Show, Eq, Ord, Generic)

--  FIXME:  [(ShardHash, Distance Point)] => Map ShardHash (Distance Point)
data SpaceSnapshot     = SpaceSnapshot {
    _shapshotHashes :: [(ShardHash, Distance Point)]
  }
  deriving (Show, Eq, Ord, Generic)


data ShardIndex = ShardIndex {
        _shardExistIndex    :: ShardExistIndex
    ,   _shardNeededIndex   :: ShardNeededIndex
    ,   _shardLoadingIndex  :: ShardLoadingIndex
  } deriving (Show, Eq, Ord, Generic)


instance Serialize SpaceSnapshot
instance Serialize ShardIndex
instance Serialize ShardExistIndex
instance Serialize ShardNeededIndex
instance Serialize ShardLoadingIndex
instance Serialize Priority

makeLenses ''SpaceSnapshot
makeLenses ''ShardExistIndex
makeLenses ''ShardIndex
makeLenses ''ShardLoadingIndex
makeLenses ''ShardNeededIndex

indexToSet :: ShardIndex -> S.Set ShardHash
indexToSet aIndexList = S.unions $ aSnapshotToSet <$>
    aIndexList^.shardExistIndex.baseSnapshots
  where
    aSnapshotToSet :: SpaceSnapshot -> S.Set ShardHash
    aSnapshotToSet (SpaceSnapshot aSnapshot) = S.fromList $ (^._1) <$> aSnapshot


addShardToIndex :: Shard -> MyNodePosition -> ShardIndex -> ShardIndex
addShardToIndex aShard aMyPosition aShardIndex = aShardIndex &~ do
    zoom shardExistIndex $ do
        lastSnapshot.shapshotHashes %= filter (\h -> h^._1 /= aHash)
        zoom baseSnapshots $ do
            _head %= addShardToSnapshot aShard aMyPosition
            _tail.traversed.shapshotHashes  %= filter (\h -> h^._1 /= aHash)
    shardNeededIndex.setOfHash              %= S.delete aHash
    shardLoadingIndex.setOfLoadingShards    %= filter (\h -> h^._1 /= aHash)
  where
    aHash   = shardToHash aShard


addShardToSnapshot :: Shard -> MyNodePosition -> SpaceSnapshot -> SpaceSnapshot
addShardToSnapshot aShard aMyNodePosition (SpaceSnapshot aListOfShard) =
    SpaceSnapshot $
        (shardToHash aShard, distanceTo aMyNodePosition aShard) : aListOfShard


cleanShardIndex :: MyNodePosition -> Distance Point -> Int -> ShardIndex -> (ShardIndex, [ShardHash])
cleanShardIndex  aMyNodePosition aRadiusOfCapture aCount aShardIndex = if
    | aSizeOfIndex < aCount                         -> (aShardIndex, [])
    | aSizeOfBaseIndex >= aCount                    -> (
            aShardIndex & shardExistIndex.lastSnapshot .~ empty
        ,   snapshotToListOfHash $ aShardIndex^.shardExistIndex.lastSnapshot)
    | otherwise                                     -> (
            aShardIndex & shardExistIndex.lastSnapshot .~ SpaceSnapshot aTaken
        ,   fst <$> aDroped <> aFilterDeleted)
  where
    (aTaken, aDroped)            = splitAt (aCount - aSizeOfBaseIndex) aFiltered
    (aFiltered , aFilterDeleted) = partition aCondition aListOfHashTheLastSnapshot

    aCondition = checkShardIsInRadiusOfCapture
        (toNodePosition aMyNodePosition)
        aRadiusOfCapture.fst

    SpaceSnapshot aListOfHashTheLastSnapshot    = aLastSnapshot
    aLastSnapshot                               = aShardIndex^.shardExistIndex.lastSnapshot
    aSizeOfIndex                                = sizeOfIndex aShardIndex
    aSizeOfBaseIndex                            = aSizeOfIndex - sizeOfSnapshot aLastSnapshot


snapshotToListOfHash :: SpaceSnapshot -> [ShardHash]
snapshotToListOfHash (SpaceSnapshot aList) = fst <$> aList


sizeOfIndex :: ShardIndex -> Int
sizeOfIndex (ShardIndex (ShardExistIndex aSnapshotList aLastSnapshot) _ _) =
    sum (sizeOfSnapshot <$> aSnapshotList) + sizeOfSnapshot aLastSnapshot


sizeOfSnapshot :: SpaceSnapshot -> Int
sizeOfSnapshot (SpaceSnapshot aSpaceSnapshots) = length aSpaceSnapshots

class RecalcIndex a where
    recalcIndex :: MyNodePosition -> a -> a


instance Monoid SpaceSnapshot where
    mempty  = empty
    mappend (SpaceSnapshot x1) (SpaceSnapshot x2) =
        SpaceSnapshot $ S.toList $ S.union (S.fromList x1) (S.fromList x2)


instance RecalcIndex ShardIndex where
    recalcIndex aPosition (ShardIndex x y z) = ShardIndex
        (recalcIndex aPosition x) y z


instance RecalcIndex SpaceSnapshot where
    recalcIndex aPosition (SpaceSnapshot aSnapshotList) =
        SpaceSnapshot $
            (\(aPoint, _) -> (aPoint, distanceTo aPosition aPoint))
            <$> aSnapshotList

instance RecalcIndex ShardExistIndex where
    recalcIndex aPosition (ShardExistIndex aIndexList aLastSnapshot) =
        ShardExistIndex
            (empty : (recalcIndex aPosition <$> aIndexList'))
            aLastSnapshot'
      where
        aLastSnapshot' :: SpaceSnapshot
        aLastSnapshot' = if
            | checkLength -> last aIndexList <> aLastSnapshot
            | otherwise   -> aLastSnapshot

        aIndexList' :: [SpaceSnapshot]
        aIndexList' = if
            | checkLength -> init aIndexList
            | otherwise   -> aIndexList

        checkLength :: Bool
        checkLength = length aIndexList < 5


shardIndexFileName :: String
shardIndexFileName = "shardDB/shardIndex.index"


loadMyShardIndex :: IO ShardIndex
loadMyShardIndex = do
    aReading <- try $ B.readFile shardIndexFileName
    case aReading of
        Right aFileData -> case decode aFileData of
            Right aShardIndex       -> return aShardIndex
            Left  _                 -> return empty
        Left (_ :: SomeException)   -> return empty


saveMyShardIndex :: ShardIndex -> IO ()
saveMyShardIndex aShardIndex = do
    B.writeFile shardIndexFileName $ encode aShardIndex

--------------------------------------------------------------------------------

class Emptable a where
    empty :: a

instance Emptable ShardIndex where
    empty = ShardIndex empty empty empty

instance Emptable ShardExistIndex where
    empty = ShardExistIndex [empty] empty

instance Emptable ShardLoadingIndex where
    empty = ShardLoadingIndex []

instance Emptable SpaceSnapshot where
    empty = SpaceSnapshot []

instance Emptable ShardNeededIndex where
    empty = ShardNeededIndex S.empty



---
