{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.RealPoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Framework.Lens               as Lens
import           Enecuum.Prelude

data TstRealPoWNode = TstRealPoWNode
    deriving (Show, Generic)

data instance NodeConfig TstRealPoWNode = TstRealPoWNodeConfig
        { _controlRpcPort      :: D.PortNumber
        , _graphNodeUDPAddress :: D.Address
        , _baseDifficulty      :: D.Difficulty
        , _workersCount        :: Word32
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig TstRealPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstRealPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   TstRealPoWNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstRealPoWNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions
