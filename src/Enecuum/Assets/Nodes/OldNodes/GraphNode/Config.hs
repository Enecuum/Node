{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Nodes.OldNodes.GraphNode.Config where

import qualified Data.Aeson                            as A
import           Enecuum.Assets.Nodes.Address
import qualified Enecuum.Assets.Nodes.GraphNode.Config as Prd
import           Enecuum.Config
import qualified Enecuum.Domain                        as D
import           Enecuum.Prelude

data OldGraphNode = OldGraphNode
    deriving (Show, Generic)

data instance NodeConfig OldGraphNode = OldGraphNodeConfig
    { _dbConfig     :: Prd.DBConfig
    , _gnNodePorts  :: NodePorts
    , _rpcSynco     :: Maybe D.Address
    }
    deriving (Show, Generic)

instance ToJSON   OldGraphNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON OldGraphNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
-- instance ToJSON   DBConfig                  where toJSON    = A.genericToJSON    nodeConfigJsonOptions
-- instance FromJSON DBConfig                  where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig OldGraphNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig OldGraphNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

-- data DBConfig = DBConfig
--     { _useDatabase         :: Bool        -- ^ If True, DB will be used to restore the state on the start and dump the state during work.
--     , _dbModelName         :: String      -- ^ DB model name. Can be a full path if useEnqHomeDir == False.
--     , _useEnqHomeDir       :: Bool        -- ^ When True, ~/.enecuum/<dbModelName> path will be used.
--     , _dbOptions           :: D.DBOptions -- ^ DB options.
--     , _stopOnDatabaseError :: Bool  -- ^ The node will stop if something wrong with DB model.
--     }
--     deriving (Show, Generic)

noDBConfig' :: Prd.DBConfig
noDBConfig' = Prd.DBConfig
    { Prd._useDatabase         = False
    , Prd._dbModelName         = ""
    , Prd._useEnqHomeDir       = False
    , Prd._dbOptions           = D.DBOptions True True
    , Prd._stopOnDatabaseError = True
    }

defaultNodeConfig :: NodeConfig OldGraphNode
defaultNodeConfig = OldGraphNodeConfig noDBConfig' defaultGnNodePorts Nothing

    -- { _dbConfig     = noDBConfig'
    -- , _gnNodePorts  = defaultGnNodePorts
    -- , _rpcSynco     = Nothing
    -- }
