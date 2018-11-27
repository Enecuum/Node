{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.PoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Framework.Lens               as Lens
import           Enecuum.Prelude

type BlocksDelay = Int

data TstPoWNode = TstPoWNode
    deriving (Show, Generic)

data instance NodeConfig TstPoWNode = TstPoWNodeConfig
        { _defaultBlocksDelay  :: BlocksDelay
        , _kblocksOrder        :: A.Ordering
        , _graphNodeUDPAddress :: D.Address
        , _powNodeRpcPort      :: D.PortNumber
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig TstPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   TstPoWNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstPoWNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

defaultPoWNodeConfig :: NodeConfig TstPoWNode
defaultPoWNodeConfig = TstPoWNodeConfig
    { _defaultBlocksDelay  = 1000 * 1000
    , _kblocksOrder        = A.InOrder
    , _graphNodeUDPAddress = getUdpAddress defaultGnNodeAddress
    , _powNodeRpcPort      = defaultPoWNodePorts ^. Lens.nodeRpcPort
    }
