{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Nodes.RoutingNodes.GenPoW.Config where

import qualified Data.Aeson                           as J
import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Domain                                     as D
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Config
import           Enecuum.Domain                       (NodeAddress (..), NodeId (..), NodePorts (..))
import           Enecuum.Prelude

type BlocksDelay = Int

data GenPoWNode = GenPoWNode
    deriving (Show, Generic)

data instance NodeConfig GenPoWNode = GenPoWNodeConfig
        { _defaultBlocksDelay  :: BlocksDelay
        , _kblocksOrder        :: A.Ordering
        , _powNodebnAddress    :: NodeAddress
        , _powNodePorts        :: NodePorts
        , _powNodeId           :: D.NodeId
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig GenPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GenPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions
instance ToJSON   GenPoWNode              where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON GenPoWNode              where parseJSON = J.genericParseJSON nodeConfigJsonOptions

defaultBlocksDelay :: BlocksDelay
defaultBlocksDelay = 1000 * 1000

routingGenPoWNodeConfig :: NodeConfig GenPoWNode
routingGenPoWNodeConfig = GenPoWNodeConfig defaultBlocksDelay A.InOrder routingBootNodeAddress routingGenPoWNodePorts (D.toHashGeneric routingGenPoWNodePorts)
