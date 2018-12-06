module Enecuum.Assets.Nodes.GraphService.Config where

import qualified Data.Aeson                   as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain               as D
import           Enecuum.Blockchain.Domain          as D
import           Enecuum.Prelude

data GraphWindowConfig = GraphWindowConfig
    { _shrinkingEnabled :: Bool
    , _shrinkingDelay   :: Int
    , _windowSize       :: D.BlockNumber
    }
    deriving (Show, Generic)

data DBConfig = DBConfig
    { _useDatabase         :: Bool
        -- ^ If True, DB will be used to restore the state on the start and dump the state during work.
    , _dbModelName         :: String
        -- ^ DB model name. Can be a full path if useEnqHomeDir == False.
    , _useEnqHomeDir       :: Bool
        -- ^ When True, ~/.enecuum/<dbModelName> path will be used.
    , _dbOptions           :: D.DBOptions
        -- ^ DB options.
    , _stopOnDatabaseError :: Bool
        -- ^ The node will stop if something wrong with DB model.
    }
    deriving (Show, Generic)

data GraphServiceConfig = GraphServiceConfig
    { _graphWindowConfig :: GraphWindowConfig
    , _dbConfig          :: DBConfig
    , _rpcSynco          :: Maybe D.Address
    }
    deriving (Show, Generic)

instance ToJSON   DBConfig           where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON DBConfig           where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   GraphWindowConfig  where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphWindowConfig  where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   GraphServiceConfig where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphServiceConfig where parseJSON = A.genericParseJSON nodeConfigJsonOptions

noDBConfig :: DBConfig
noDBConfig = DBConfig
    { _useDatabase         = False
    , _dbModelName         = ""
    , _useEnqHomeDir       = False
    , _dbOptions           = D.DBOptions True True
    , _stopOnDatabaseError = True
    }

noGraphShrinking :: GraphWindowConfig
noGraphShrinking = GraphWindowConfig
    { _shrinkingEnabled = False
    , _shrinkingDelay = 1000 * 1000
    , _windowSize = 10
    }
