{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Framework.Lens        as Lens

import           Enecuum.Framework.TestData.RPC
import qualified Enecuum.Framework.TestData.TestGraph as TG
import qualified Enecuum.Framework.Domain.Types as T

bootNodeAddr, masterNode1Addr :: D.NodeAddress
bootNodeAddr = "boot node addr"
masterNode1Addr = "master node 1 addr"

networkNode1Addr, networkNode2Addr :: D.NodeAddress
networkNode1Addr = "networkNode1Addr"
networkNode2Addr = "networkNode2Addr"

bootNodeTag, masterNodeTag :: D.NodeTag
bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

-- | Boot node discovery sample scenario.
-- Currently, does nothing but returns the default boot node address.
simpleBootNodeDiscovery :: Eff L.NetworkModel D.NodeAddress
simpleBootNodeDiscovery = pure bootNodeAddr

-- RPC handlers.

acceptHello1 :: HelloRequest1 -> Eff L.NodeModel HelloResponse1
acceptHello1 (HelloRequest1 msg) = pure $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: HelloRequest2 -> Eff L.NodeModel HelloResponse2
acceptHello2 (HelloRequest2 msg) = pure $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: GetHashIDRequest -> Eff L.NodeModel GetHashIDResponse
acceptGetHashId GetHashIDRequest = pure $ GetHashIDResponse "1"

-- Boot node scenario

bootNode :: Eff L.NodeDefinitionModel ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.serving
    $ L.serve @HelloRequest1 @HelloResponse1 acceptHello1
    . L.serve @GetHashIDRequest @GetHashIDResponse acceptGetHashId

-- Master node scenario

masterNodeInitialization :: Eff L.NodeModel (Either Text D.NodeID)
masterNodeInitialization = do
  addr     <- L.evalNetwork simpleBootNodeDiscovery
  eHashID  <- fmap unpack <$> L.withConnection (D.ConnectionConfig addr) GetHashIDRequest
  pure $ eHashID >>= Right . D.NodeID

masterNode :: Eff L.NodeDefinitionModel ()
masterNode = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
  L.serving
    $ L.serve @HelloRequest1 @HelloResponse1 acceptHello1
    . L.serve @HelloRequest2 @HelloResponse2 acceptHello2

-- Some network nodes scenarios

acceptGetBalance
  :: T.LGraphNode
  -> GetBalanceRequest
  -> Eff L.NodeModel GetBalanceResponse
acceptGetBalance txBaseNode GetBalanceRequest = do
  pure $ GetBalanceResponse (-1)

acceptBalanceChange
  :: T.LGraphNode
  -> BalanceChangeRequest
  -> Eff L.NodeModel BalanceChangeResponse
acceptBalanceChange = error "acceptBalanceChange not implemented."

newtorkNode1Initialization :: Eff L.NodeModel T.LGraphNode
newtorkNode1Initialization = L.evalGraphAction $ TG.getTransactionNode TG.nilTransaction >>= \case
  Nothing -> error "Graph is not ready: no genesis node found."
  Just txNode -> pure txNode

networkNode1 :: Eff L.NodeDefinitionModel ()
networkNode1 = do
  L.nodeTag "networkNode1"
  txBaseNode <- L.initialization newtorkNode1Initialization
  L.serving
    $ L.serve (acceptGetBalance txBaseNode)
    . L.serve (acceptBalanceChange txBaseNode)

networkNode2Scenario :: Eff L.NodeModel ()
networkNode2Scenario = do
    balance <- D.withSuccess 
      $ fmap unpack <$> L.withConnection (D.ConnectionConfig networkNode1Addr) GetBalanceRequest
    L.logInfo $ "Current balance: " +|| balance ||+ "."

networkNode2 :: Eff L.NodeDefinitionModel ()
networkNode2 = do
  L.nodeTag "networkNode2"
  L.scenario networkNode2Scenario
