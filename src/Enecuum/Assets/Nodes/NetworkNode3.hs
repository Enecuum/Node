{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.NetworkNode3 where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import           Control.Lens                  (makeFieldsNoPrefix)

import           Enecuum.Config                (Config)
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import qualified Data.Text as Text


import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Types

acceptChainLength
  :: NetworkNodeChainData
  -> GetChainLengthRequest
  -> L.NodeL GetChainLengthResponse
acceptChainLength nodeData GetChainLengthRequest =
  GetChainLengthResponse <$> length <$> (L.atomically $ L.readVar (nodeData ^. chainVar))

acceptChainFrom
  :: NetworkNodeChainData
  -> GetChainFromRequest
  -> L.NodeL GetChainFromResponse
acceptChainFrom nodeData (GetChainFromRequest from) =
  GetChainFromResponse <$> drop from <$> (L.atomically $ L.readVar (nodeData ^. chainVar))


newtorkNode3Initialization :: L.NodeL NetworkNodeChainData
newtorkNode3Initialization = do
  (_, blocks) <- D.generateNKBlocks 15
  chainVar' <- L.atomically $ L.newVar blocks
  pure $ NetworkNodeChainData chainVar'

networkNode3 :: L.NodeDefinitionL ()
networkNode3 = do
  L.nodeTag "networkNode3"
  nodeData <- L.initialization $ newtorkNode3Initialization
  L.serving 2003 $ do
    L.method (acceptChainLength nodeData)
    L.method (acceptChainFrom nodeData)
