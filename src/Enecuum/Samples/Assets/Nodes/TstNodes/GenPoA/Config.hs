{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Assets.Nodes.TstNodes.GenPoA.Config where

import qualified Data.Aeson                   as A
import           Enecuum.Samples.Assets.Nodes.Address
import           Enecuum.Config
import qualified Enecuum.Domain               as D
import qualified Enecuum.Framework.Lens       as Lens
import           Enecuum.Prelude


data TstGenPoANode = TstGenPoANode
    deriving (Show, Generic)

data instance NodeConfig TstGenPoANode = TstGenPoANodeConfig
    { _controlRpcPort :: D.PortNumber
    , _genPoaGraphNodeUDPAddress :: D.Address
    , _genPoaGraphNodeRPCAddress :: D.Address
    }
    deriving (Show, Generic)

instance ToJSON   TstGenPoANode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstGenPoANode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstGenPoANode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstGenPoANode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions

tstGenPoANodeConfig :: NodeConfig TstGenPoANode
tstGenPoANodeConfig = TstGenPoANodeConfig
    (tstGenPoANodePorts ^. Lens.nodeRpcPort)
    (getUdpAddress tstGraphNodeTransmitterAddress)
    (getRpcAddress tstGraphNodeTransmitterAddress)
