{-# LANGUAGE DuplicateRecordFields #-}
-- | Lenses for node configs.
module Enecuum.Assets.Nodes.OldNodes.GraphNode.CLens where

import           Control.Lens                                   (Getter, to)
import           Enecuum.Prelude

import qualified Enecuum.Assets.Nodes.GraphNode.Config          as Prd
import           Enecuum.Assets.Nodes.OldNodes.GraphNode.Config
import           Enecuum.Config
import qualified Enecuum.Domain                                 as D
-- import           Enecuum.Assets.Nodes.PoW.Config as PoW
-- import           Enecuum.Assets.Nodes.OldNodes.PoW.Config

useDatabase :: Getter (NodeConfig OldGraphNode) Bool
useDatabase = to (Prd._useDatabase . _dbConfig)

dbModelName :: Getter (NodeConfig OldGraphNode) FilePath
dbModelName = to (Prd._dbModelName . _dbConfig)

useEnqHomeDir :: Getter (NodeConfig OldGraphNode) Bool
useEnqHomeDir = to (Prd._useEnqHomeDir . _dbConfig)

dbOptions :: Getter (NodeConfig OldGraphNode) D.DBOptions
dbOptions = to (Prd._dbOptions . _dbConfig)

stopOnDatabaseError :: Getter (NodeConfig OldGraphNode) Bool
stopOnDatabaseError = to (Prd._stopOnDatabaseError . _dbConfig)

-- defaultBlocksDelay :: Getter (NodeConfig OldPoWNode) PoW.BlocksDelay
-- defaultBlocksDelay = to PoW._defaultBlocksDelay
