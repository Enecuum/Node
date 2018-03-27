{-# LANGUAGE TemplateHaskell, DeriveGeneric, ScopedTypeVariables, MultiWayIf #-}

module Sharding.ShardDB.ShardIndex where

import              Control.Exception
import              Lens.Micro.TH
import              System.Clock
import              Data.Serialize
import qualified    Data.ByteString as B
import              Data.Monoid

import qualified    Data.Set as S
import              GHC.Generics

import              Sharding.Types.Shard
import              Sharding.Space.Distance
import              Sharding.Space.Point

data ShardExistIndex   = ShardExistIndex [SpaceSnapshot] SpaceSnapshot
  deriving (Show, Eq, Ord, Generic)

data ShardNeededIndex  = ShardNeededIndex [ShardHash]
  deriving (Show, Eq, Ord, Generic)

data ShardLoadingIndex = ShardLoadingIndex [(ShardHash, Priority, TimeSpec)]
  deriving (Show, Eq, Ord, Generic)

data Priority          = Priority Int
  deriving (Show, Eq, Ord, Generic)

data SpaceSnapshot     = SpaceSnapshot [(ShardHash, Distance Point)]
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


makeLenses ''ShardIndex


addShardToIndex :: Shard -> MyNodePosition -> ShardIndex -> ShardIndex
addShardToIndex aShard aMyPosition aShardIndex = undefined


cleanShardIndex :: MyNodePosition -> Distance Point -> Int -> ShardIndex -> ShardIndex
cleanShardIndex aMyNodePosition aDistance aCount aShardIndex = undefined

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
    empty = ShardExistIndex [] empty

instance Emptable ShardLoadingIndex where
    empty = ShardLoadingIndex []

instance Emptable SpaceSnapshot where
    empty = SpaceSnapshot []

instance Emptable ShardNeededIndex where
    empty = ShardNeededIndex []



---
