{-# LANGUAGE TemplateHaskell, DeriveGeneric, ScopedTypeVariables #-}

module Sharding.ShardDB.ShardIndex where

import              Control.Exception
import              Lens.Micro.TH
import              System.Clock
import              Data.Serialize
import qualified    Data.ByteString as B

import              GHC.Generics

import              Sharding.Types.Shard
import              Sharding.Space.Distance
import              Sharding.Space.Point

data ShardExistIndex   = ShardExistIndex [SpaceSnapshot]
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

addSpaceSnapshot :: SpaceSnapshot -> MyNodePosition -> ShardIndex -> ShardIndex
addSpaceSnapshot aSpaceSnapshot aMyPosition aShardIndex = undefined


addShardToIndex :: Shard -> MyNodePosition -> ShardIndex -> ShardIndex
addShardToIndex aShard aMyPosition aShardIndex = undefined


shardIndexFileName :: String
shardIndexFileName = "shardDB/shardIndex.index"

emptyShardIndex = undefined

loadMyShardIndex :: IO ShardIndex
loadMyShardIndex = do
    aReading <- try $ B.readFile shardIndexFileName
    case aReading of
        Right aFileData -> case decode aFileData of
            Right aShardIndex       -> return aShardIndex
            Left  _                 -> return emptyShardIndex
        Left (_ :: SomeException)   -> return emptyShardIndex


saveMyShardIndex :: ShardIndex -> IO ()
saveMyShardIndex aShardIndex = do
    B.writeFile shardIndexFileName $ encode aShardIndex






---
