{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.GenPoA.Config where

import qualified Data.Aeson      as A
import           Enecuum.Config
import qualified Enecuum.Domain  as D
import           Enecuum.Prelude


data TstGenPoANode = TstGenPoANode
    deriving (Show, Generic)

data instance NodeConfig TstGenPoANode = TstGenPoANodeConfig
    { _poaRPCPort  :: D.PortNumber
    }
    deriving (Show, Generic)

instance ToJSON   TstGenPoANode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstGenPoANode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstGenPoANode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstGenPoANode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
