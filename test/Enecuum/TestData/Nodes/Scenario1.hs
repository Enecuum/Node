{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.TestData.Nodes.Scenario1 where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Control.Lens                  (makeFieldsNoPrefix)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Enecuum.Language              (HasGraph)

import qualified Enecuum.Core.HGraph.Internal.Types as T

import           Enecuum.TestData.RPC
import qualified Enecuum.TestData.TestGraph as TG
import           Enecuum.TestData.Nodes.Address

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionL ()
bootNode = do
    L.nodeTag bootNodeTag
    L.serving 2000 $ do
        L.method acceptHello1
        L.method acceptGetHashId


simpleBootNodeDiscovery :: L.NetworkL D.Address
simpleBootNodeDiscovery = pure bootNodeAddr

masterNodeInitialization :: L.NodeL (Either Text D.NodeID)
masterNodeInitialization = do
    addr                      <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
    GetHashIDResponse eHashID <- L.makeRpcRequestUnsafe addr GetHashIDRequest
    pure $ Right (D.NodeID eHashID)

masterNode :: L.NodeDefinitionL ()
masterNode = do
    L.nodeTag masterNodeTag
    nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
    L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
    L.serving 2000 $ do
        L.method acceptHello1
        L.method acceptHello2
