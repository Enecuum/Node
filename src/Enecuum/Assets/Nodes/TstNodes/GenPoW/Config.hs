{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.GenPoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Framework.Lens               as Lens
import           Enecuum.Prelude

type BlocksDelay = Int

data TstGenPoWNode = TstGenPoWNode
    deriving (Show, Generic)

data instance NodeConfig TstGenPoWNode = TstGenPoWNodeConfig
        { _defaultBlocksDelay        :: BlocksDelay
        , _kblocksOrder              :: A.Ordering
        , _genPowGraphNodeUDPAddress :: D.Address
        , _controlRpcPort            :: D.PortNumber
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig TstGenPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstGenPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   TstGenPoWNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstGenPoWNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

tstGenPoWNodeConfig :: NodeConfig TstGenPoWNode
tstGenPoWNodeConfig = TstGenPoWNodeConfig
    { _defaultBlocksDelay        = 1000 * 1000
    , _kblocksOrder              = A.InOrder
    , _genPowGraphNodeUDPAddress = getUdpAddress tstGraphNodeTransmitterAddress
    , _controlRpcPort            = tstGenPoWNodePorts ^. Lens.nodeRpcPort
    }
