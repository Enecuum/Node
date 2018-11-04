{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW.Config where

import           Enecuum.Prelude
import           Enecuum.Config
import qualified Data.Aeson as A

type BlocksDelay = Int

data PoWNode = PoWNode
    deriving (Show, Generic)

data instance NodeConfig PoWNode = PoWNodeConfig
        { _defaultBlocksDelay :: BlocksDelay
        }
    deriving (Show, Generic)

instance ToJSON   (NodeConfig PoWNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PoWNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   PoWNode              where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON PoWNode              where parseJSON = A.genericParseJSON nodeConfigJsonOptions
