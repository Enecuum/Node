module Sharding.ShardDB.ShardStore where

import Data.Map as M
import Sharding.Types.Node
import Sharding.Types.Shard
import Sharding.Space.Point
import Sharding.Space.Distance



-- TODO Is it file or db like sqlite?
loadShards :: IO (M.Map ShardHash Shard)
loadShards = undefined

{-
saveShard :: Shard -> (ShardingNode ->  IO ()) -> ShardingNode -> IO ()
saveShard aShard aLoop aShardingNode = undefined
-}


---
