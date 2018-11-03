-- | Lenses for node configs.
module Enecuum.Assets.Nodes.CLens where

import           Enecuum.Prelude
import           Control.Lens (Getter, to)

import           Enecuum.Config
import qualified Enecuum.Domain as D
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.PoW.Config

useDatabase :: Getter (NodeConfig GraphNode) Bool
useDatabase = to _useDatabase

dbModelName :: Getter (NodeConfig GraphNode) FilePath
dbModelName = to _dbModelName

useEnqHomeDir :: Getter (NodeConfig GraphNode) Bool
useEnqHomeDir = to _useEnqHomeDir

dbOptions :: Getter (NodeConfig GraphNode) D.DBOptions
dbOptions = to _dbOptions

stopOnDatabaseError :: Getter (NodeConfig GraphNode) Bool
stopOnDatabaseError = to _stopOnDatabaseError

defaultBlocksDelay :: Getter (NodeConfig PoWNode) BlocksDelay
defaultBlocksDelay = to _defaultBlocksDelay
