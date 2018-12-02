{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.TestData.Nodes.Scenario1 where

import           Enecuum.Prelude

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.TestData.RPC
import           Enecuum.TestData.Nodes.Address

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionL ()
bootNode = do
    L.setNodeTag bootNodeTag
    void $ L.serving D.Rpc 2000 $ do
        L.method acceptHello1
        L.method acceptGetHashId


simpleBootNodeDiscovery :: L.NodeL D.Address
simpleBootNodeDiscovery = pure bootNodeAddr

masterNodeInitialization :: L.NodeL (Either Text D.NodeID)
masterNodeInitialization = do
    addr                      <- simpleBootNodeDiscovery
    GetHashIDResponse eHashID <- L.makeRpcRequestUnsafe addr GetHashIDRequest
    pure $ Right (D.NodeID eHashID)

masterNode :: L.NodeDefinitionL ()
masterNode = do
    L.setNodeTag masterNodeTag
    nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
    L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
    void $ L.serving D.Rpc 2000 $ do
        L.method acceptHello1
        L.method acceptHello2
