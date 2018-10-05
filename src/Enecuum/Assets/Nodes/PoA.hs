{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoA where

import           Data.HGraph.StringHashable   (toHash)
import           Enecuum.Assets.Nodes.Address (poaAddr)
import qualified Enecuum.Domain               as D
import qualified Enecuum.Language             as L
import           Enecuum.Prelude


data MicroblockRequest  = MicroblockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data MicroblockResponse = MicroblockResponse { tx :: D.Transaction }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

getMicroblock:: MicroblockRequest -> L.NodeL MicroblockResponse
getMicroblock MicroblockRequest =  do
  L.logInfo "=="  
  undefined

poaNode :: L.NodeDefinitionL ()
poaNode = do
    L.nodeTag "PoA node"
    L.logInfo "Get Microblocks"
    let (D.Address _ port) = poaAddr
    L.servingRpc port $ do
      L.method $ getMicroblock


