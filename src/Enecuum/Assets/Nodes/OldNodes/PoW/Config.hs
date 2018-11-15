{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Assets.Nodes.OldNodes.PoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address         (graphNodeTransmitterUdpAddress, powNodeRpcPort)
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import           Enecuum.Prelude

type BlocksDelay = Int

data OldPoWNode = OldPoWNode
    deriving (Show, Generic)

data instance NodeConfig OldPoWNode = OldPoWNodeConfig
        { _defaultBlocksDelay  :: BlocksDelay
        , _kblocksOrder        :: A.Ordering
        , _graphNodeUDPAddress :: D.Address
        , _powNodeRpcPort      :: D.PortNumber
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig OldPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig OldPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   OldPoWNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON OldPoWNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

defaultBlocksDelay :: BlocksDelay
defaultBlocksDelay = 1000 * 1000

defaultPoWNodeConfig = OldPoWNodeConfig defaultBlocksDelay A.InOrder graphNodeTransmitterUdpAddress powNodeRpcPort
