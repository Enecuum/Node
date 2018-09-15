{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens

import           Enecuum.Framework.TestData.RPC
import qualified Enecuum.Framework.TestData.TestGraph as TG
import qualified Enecuum.Framework.Domain.Types as T

makeRequestUnsafe
  :: Member L.NetworkingL effs
  => D.RpcMethod () req resp
  => D.ConnectionConfig
  -> req
  -> Eff effs resp
makeRequestUnsafe cfg = D.withSuccess . L.withConnection cfg

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

-- Scenario 1: master node can interact with boot node.

bootNode :: Eff L.NodeDefinitionModel ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.serving
    $ L.serve @HelloRequest1 @HelloResponse1 acceptHello1
    . L.serve @GetHashIDRequest @GetHashIDResponse acceptGetHashId

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

-- Scenario 2: 2 network nodes can interact.
-- One holds a graph with transactions. Other requests balance and amount change.

-- In this scenario, we assume the graph is list-like.
calculateBalance
  :: L.LGraphNode
  -> Int
  -> Eff L.LGraphModel Int
calculateBalance curNode curBalance = do
  let trans = D.fromContent $ curNode ^. Lens.content
  let balanceChange = trans ^. Lens.change
  let links = curNode ^. Lens.links
  case Map.toList links of
    []                    -> pure $ curBalance + balanceChange
    [(nextHash, nextRef)] -> L.getNode nextRef >>= \case
      Just nextNode -> calculateBalance nextNode $ curBalance + balanceChange
      Nothing       -> error "Invalid reference found."
    _ -> error "In this test scenario, graph should be list-like."

tryAddTransaction'
  :: L.LGraphNode
  -> Int
  -> Int
  -> Eff L.LGraphModel (Maybe Int)
tryAddTransaction' lastNode lastBalance change
  | lastBalance + change < 0 = pure Nothing
  | otherwise = do
      let newTrans = D.Transaction (lastNode ^. Lens.hash) change
      L.newNode newTrans
      mbNewNode <- L.getNode $ D.toHash newTrans
      case mbNewNode of
        Nothing -> error "Node insertion failed."
        Just newNode -> do
          L.newLink (lastNode ^. Lens.ref) (newNode ^. Lens.ref)
          pure $ Just $ lastBalance + change

tryAddTransaction
  :: L.LGraphNode
  -> Int
  -> Int
  -> Eff L.LGraphModel (Maybe Int)
tryAddTransaction curNode prevBalance change = do
  let trans = D.fromContent $ curNode ^. Lens.content
  let curBalanceChange = trans ^. Lens.change
  let curBalance = prevBalance + curBalanceChange
  let links = curNode ^. Lens.links
  case Map.toList links of
    []                    -> tryAddTransaction' curNode curBalance change
    [(nextHash, nextRef)] -> L.getNode nextRef >>= \case
      Just nextNode -> tryAddTransaction nextNode curBalance change
      Nothing       -> error "Invalid reference found."
    _ -> error "In this test scenario, graph should be list-like."

acceptGetBalance
  :: L.LGraphNode
  -> GetBalanceRequest
  -> Eff L.NodeModel GetBalanceResponse
acceptGetBalance baseNode GetBalanceRequest = do
  balance <- L.evalGraph (calculateBalance baseNode 0)
  pure $ GetBalanceResponse balance

acceptBalanceChange
  :: L.LGraphNode
  -> BalanceChangeRequest
  -> Eff L.NodeModel BalanceChangeResponse
acceptBalanceChange baseNode (BalanceChangeRequest change) = do
  mbBalance <- L.evalGraph $ tryAddTransaction baseNode 0 change
  pure $ BalanceChangeResponse mbBalance

newtorkNode1Initialization :: Eff L.NodeModel L.LGraphNode
newtorkNode1Initialization = L.evalGraph $ TG.getTransactionNode TG.nilTransaction >>= \case
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
    let connectCfg = D.ConnectionConfig networkNode1Addr
    -- No balance change
    balance0 <- unpack <$> makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance0 (should be 0): " +|| balance0 ||+ "."
    -- Add 10
    balance1 <- unpack <$> (makeRequestUnsafe connectCfg $ BalanceChangeRequest 10)
    L.logInfo $ "balance1 (should be Just 10): " +|| balance1 ||+ "."
    -- Subtract 20
    balance2 <- unpack <$> (makeRequestUnsafe connectCfg $ BalanceChangeRequest (-20))
    L.logInfo $ "balance2 (should be Nothing): " +|| balance2 ||+ "."
    -- Add 101
    balance3 <- unpack <$> (makeRequestUnsafe connectCfg $ BalanceChangeRequest 101)
    L.logInfo $ "balance3 (should be Just 111): " +|| balance3 ||+ "."

networkNode2 :: Eff L.NodeDefinitionModel ()
networkNode2 = do
  L.nodeTag "networkNode2"
  L.scenario networkNode2Scenario
