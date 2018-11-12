{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.PoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address         (graphNodeTransmitterUdpAddress, powNodeRpcPort)
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import           Enecuum.Prelude

type BlocksDelay = Int

data PoWNode = PoWNode
    deriving (Show, Generic)

data instance NodeConfig PoWNode = PoWNodeConfig
        { _defaultBlocksDelay  :: BlocksDelay
        , _kblocksOrder        :: A.Ordering
        , _graphNodeUDPAddress :: D.Address
        , _powNodeRpcPort      :: D.PortNumber
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig PoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   PoWNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON PoWNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

defaultBlocksDelay :: BlocksDelay
defaultBlocksDelay = 1000 * 1000

defaultPoWNodeConfig = PoWNodeConfig defaultBlocksDelay A.InOrder graphNodeTransmitterUdpAddress powNodeRpcPort
