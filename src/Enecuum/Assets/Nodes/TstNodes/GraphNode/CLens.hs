{-# LANGUAGE DuplicateRecordFields #-}
-- | Lenses for node configs.
module Enecuum.Assets.Nodes.TstNodes.GraphNode.CLens where

import           Control.Lens                                   (Getter, to)
import           Enecuum.Prelude

import qualified Enecuum.Assets.Nodes.GraphNode.Config          as Prd
import           Enecuum.Assets.Nodes.TstNodes.GraphNode.Config
import           Enecuum.Config
import qualified Enecuum.Domain                                 as D
-- import           Enecuum.Assets.Nodes.PoW.Config as PoW
-- import           Enecuum.Assets.Nodes.TstNodes.PoW.Config

useDatabase :: Getter (NodeConfig TstGraphNode) Bool
useDatabase = to (Prd._useDatabase . _dbConfig)

dbModelName :: Getter (NodeConfig TstGraphNode) FilePath
dbModelName = to (Prd._dbModelName . _dbConfig)

useEnqHomeDir :: Getter (NodeConfig TstGraphNode) Bool
useEnqHomeDir = to (Prd._useEnqHomeDir . _dbConfig)

dbOptions :: Getter (NodeConfig TstGraphNode) D.DBOptions
dbOptions = to (Prd._dbOptions . _dbConfig)

stopOnDatabaseError :: Getter (NodeConfig TstGraphNode) Bool
stopOnDatabaseError = to (Prd._stopOnDatabaseError . _dbConfig)

-- defaultBlocksDelay :: Getter (NodeConfig TstPoWNode) PoW.BlocksDelay
-- defaultBlocksDelay = to PoW._defaultBlocksDelay
