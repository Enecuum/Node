{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Assets.Nodes.TstNodes.PoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
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

defaultBlocksDelay :: BlocksDelay
defaultBlocksDelay = 1000 * 1000

defaultPoWNodeConfig :: NodeConfig TstPoWNode
defaultPoWNodeConfig = TstPoWNodeConfig
    defaultBlocksDelay
    A.InOrder
    (getUdpAddress defaultGnNodeAddress)
    (defaultPoWNodePorts ^. nodeRpcPort)
