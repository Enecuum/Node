module Enecuum.Assets.Nodes.GraphNode.Config where

import qualified Data.Aeson                   as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain               as D
import           Enecuum.Prelude

data GraphNode = GraphNode
    deriving (Show, Generic)

data instance NodeConfig GraphNode = GraphNodeConfig
    { _dbConfig :: DBConfig
    , _udpPort :: D.PortNumber
    , _tcpPort :: D.PortNumber
    , _rpcPort :: D.PortNumber
    }
    deriving (Show, Generic)

data DBConfig = DBConfig
    { _useDatabase         :: Bool        -- ^ If True, DB will be used to restore the state on the start and dump the state during work.
    , _dbModelName         :: String      -- ^ DB model name. Can be a full path if useEnqHomeDir == False.
    , _useEnqHomeDir       :: Bool        -- ^ When True, ~/.enecuum/<dbModelName> path will be used.
    , _dbOptions           :: D.DBOptions -- ^ DB options.
    , _stopOnDatabaseError :: Bool  -- ^ The node will stop if something wrong with DB model.
    }
    deriving (Show, Generic)

instance ToJSON   GraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   DBConfig               where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON DBConfig               where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig GraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

noDBConfig' :: DBConfig
noDBConfig' = DBConfig
    { _useDatabase         = False
    , _dbModelName         = ""
    , _useEnqHomeDir       = False
    , _dbOptions           = D.DBOptions True True
    , _stopOnDatabaseError = True
    }

defaultNodeConfig :: NodeConfig GraphNode
defaultNodeConfig = GraphNodeConfig
    { _dbConfig = noDBConfig'
    , _udpPort = graphNodeTransmitterUdpPort
    , _tcpPort = graphNodeTransmitterTcpPort
    , _rpcPort = graphNodeTransmitterRpcPort
    }
