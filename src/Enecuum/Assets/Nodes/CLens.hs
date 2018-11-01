-- | Lenses for node configs.
module Enecuum.Assets.Nodes.CLens where

import           Enecuum.Prelude
import           Control.Lens (Getter, to)

import           Enecuum.Config
import qualified Enecuum.Domain as D
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.PoW.Config


dbModel :: Getter (NodeConfig GraphNode) FilePath
dbModel = to _dbModel

dbOptions :: Getter (NodeConfig GraphNode) D.DBOptions
dbOptions = to _dbOptions

useDatabase :: Getter (NodeConfig GraphNode) Bool
useDatabase = to _useDatabase

delaysEnabled:: Getter (NodeConfig PoWNode) Bool
delaysEnabled = to _delaysEnabled