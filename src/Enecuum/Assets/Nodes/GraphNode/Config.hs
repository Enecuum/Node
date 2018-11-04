module Enecuum.Assets.Nodes.GraphNode.Config where

import           Enecuum.Prelude

import qualified Data.Aeson as A

import           Enecuum.Config
import qualified Enecuum.Domain as D

data GraphNode = GraphNode
    deriving (Show, Generic)

data instance NodeConfig GraphNode = GraphNodeConfig
    { _useDatabase   :: Bool        -- ^ If True, DB will be used to restore the state on the start and dump the state during work.
    , _dbModelName   :: String      -- ^ DB model name. Can be a full path if useEnqHomeDir == False.
    , _useEnqHomeDir :: Bool        -- ^ When True, ~/home/.enecuum/<dbModelName> path will be used.
    , _dbOptions     :: D.DBOptions -- ^ DB options.
    , _stopOnDatabaseError :: Bool  -- ^ The node will stop if something wrong with DB model.
    }
    deriving (Show, Generic)

instance ToJSON   GraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig GraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

noDBConfig :: NodeConfig GraphNode
noDBConfig = GraphNodeConfig
    { _useDatabase         = False
    , _dbModelName         = ""
    , _useEnqHomeDir       = False
    , _dbOptions           = D.DBOptions True True
    , _stopOnDatabaseError = True
    }