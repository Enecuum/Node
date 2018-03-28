{-# LANGUAGE TemplateHaskell, DeriveGeneric, ScopedTypeVariables, MultiWayIf #-}
module Sharding.ShardDB.ShardStore where

import Data.Map as M
import Data.Serialize
import Data.ByteString as B
import Data.String
import Data.Hex
import Control.Exception
import Data.Monoid
import Data.Maybe
import Control.Monad
import System.Directory

import Node.Crypto

import Sharding.Types.Node
import Sharding.Types.Shard
import Sharding.Space.Point
import Sharding.Space.Distance

class ShardName a where
    shardsPath :: a -> String
    shardName  :: a -> String

    shardsPath aElem = "shardDB/shard_" <> shardName aElem

instance ShardName Shard where
    shardName (Shard _ aData) = show (hex (cryptoHash aData)) <> ".block"

instance ShardName ShardHash where
    shardName (ShardHash _ x1 x2 x3 x4 x5 x6 x7 x8) =
        show (hex (encode (x1, x2, x3, x4, x5, x6, x7, x8))) <> ".block"


-- TODO Is it file or db like sqlite?
loadShards :: [ShardHash] -> IO [Shard]
loadShards aHashList = pure . catMaybes =<< forM aHashList loadShard


loadShard :: ShardHash -> IO (Maybe Shard)
loadShard aShardHash = do
    aReading <- try $ B.readFile $ shardsPath aShardHash
    case aReading of
        Right aFileData -> case decode aFileData of
            Right aShard            -> pure.pure $ aShard
            Left  _                 -> return Nothing
        Left (_ :: SomeException)   -> return Nothing


saveShard :: Shard -> IO ()
saveShard aShard = do
    B.writeFile (shardsPath aShard) $ encode aShard


removeShard :: ShardHash -> IO ()
removeShard aShardHash = removeFile (shardsPath aShardHash)



---
