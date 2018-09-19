{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import           Control.Lens.TH               (makeLenses)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens

import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Framework.TestData.RPC
import qualified Enecuum.Framework.TestData.TestGraph as TG
import qualified Enecuum.Framework.Domain.Types as T

makeRequestSafe
  :: D.RpcMethod () req resp
  => D.ConnectionConfig
  -> req
  -> L.NodeModel (D.RpcResult resp)
makeRequestSafe cfg = L.evalNetworking . L.withConnection cfg

makeRequestUnsafe
  :: D.RpcMethod () req resp
  => D.ConnectionConfig
  -> req
  -> L.NodeModel resp
makeRequestUnsafe cfg = D.withSuccess . L.evalNetworking . L.withConnection cfg

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
simpleBootNodeDiscovery :: L.NetworkModel D.NodeAddress
simpleBootNodeDiscovery = pure bootNodeAddr

-- RPC handlers.

acceptHello1 :: HelloRequest1 -> L.NodeModel HelloResponse1
acceptHello1 (HelloRequest1 msg) = pure $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: HelloRequest2 -> L.NodeModel HelloResponse2
acceptHello2 (HelloRequest2 msg) = pure $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: GetHashIDRequest -> L.NodeModel GetHashIDResponse
acceptGetHashId GetHashIDRequest = pure $ GetHashIDResponse "1"

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionModel ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.serving
    $ L.serve @HelloRequest1 @HelloResponse1 acceptHello1
    . L.serve @GetHashIDRequest @GetHashIDResponse acceptGetHashId

masterNodeInitialization :: L.NodeModel (Either Text D.NodeID)
masterNodeInitialization = do
  addr     <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  eHashID  <- unpack <<$>> makeRequestSafe (D.ConnectionConfig addr) GetHashIDRequest
  pure $ eHashID >>= Right . D.NodeID

masterNode :: L.NodeDefinitionModel ()
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
calculateBalanceTraversing
  :: D.StringHash
  -> TG.Balance
  -> L.GraphModel TG.Balance
calculateBalanceTraversing curNodeHash curBalance =
  L.getNode curNodeHash >>= \case
    Nothing -> error "Invalid reference found."
    Just curNode -> do
      let balanceChange = (D.fromContent $ curNode ^. Lens.content) ^. Lens.change
      case Map.toList (curNode ^. Lens.links) of
        []                  -> pure $ curBalance + balanceChange
        [(nextNodeHash, _)] -> calculateBalanceTraversing nextNodeHash $ curBalance + balanceChange
        _                   -> error "In this test scenario, graph should be list-like."

tryAddTransactionTraversing
  :: D.StringHash
  -> TG.Balance
  -> TG.BalanceChange
  -> L.GraphModel (Maybe (D.StringHash, TG.Balance))
tryAddTransactionTraversing curNodeHash prevBalance change =
  L.getNode curNodeHash >>= \case
    Nothing -> error "Invalid reference found."
    Just curNode -> do
      let curBalanceChange = (D.fromContent $ curNode ^. Lens.content) ^. Lens.change
      let curBalance = prevBalance + curBalanceChange
      case Map.toList (curNode ^. Lens.links) of
        []                  -> TG.tryAddTransaction' (curNode ^. Lens.hash) curBalance change
        [(nextNodeHash, _)] -> tryAddTransactionTraversing nextNodeHash curBalance change
        _                   -> error "In this test scenario, graph should be list-like."

acceptGetBalanceTraversing
  :: TNodeL D.Transaction
  -> GetBalanceRequest
  -> L.NodeModel GetBalanceResponse
acceptGetBalanceTraversing baseNode GetBalanceRequest = do
  balance <- L.evalGraph (calculateBalanceTraversing (baseNode ^. Lens.hash) 0)
  pure $ GetBalanceResponse balance

acceptBalanceChangeTraversing
  :: TNodeL D.Transaction
  -> BalanceChangeRequest
  -> L.NodeModel BalanceChangeResponse
acceptBalanceChangeTraversing baseNode (BalanceChangeRequest change) = do
  mbHashAndBalance <- L.evalGraph $ tryAddTransactionTraversing (baseNode ^. Lens.hash) 0 change
  case mbHashAndBalance of
    Nothing -> pure $ BalanceChangeResponse Nothing
    Just (D.StringHash _, balance) -> pure $ BalanceChangeResponse $ Just balance

newtorkNode1Initialization :: L.NodeModel (TNodeL D.Transaction)
newtorkNode1Initialization = L.evalGraph $ L.getNode TG.nilTransactionHash >>= \case
  Nothing -> error "Graph is not ready: no genesis node found."
  Just baseNode -> pure baseNode

networkNode1 :: L.NodeDefinitionModel ()
networkNode1 = do
  L.nodeTag "networkNode1"
  baseNode <- L.initialization newtorkNode1Initialization
  L.serving
    $ L.serve (acceptGetBalanceTraversing baseNode)
    . L.serve (acceptBalanceChangeTraversing baseNode)

networkNode2Scenario :: L.NodeModel ()
networkNode2Scenario = do
    let connectCfg = D.ConnectionConfig networkNode1Addr
    -- No balance change
    balance0 <- unpack <$> makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance0 (should be 0): " +|| balance0 ||+ "."
    -- Add 10
    balance1 <- unpack <$> makeRequestUnsafe connectCfg (BalanceChangeRequest 10)
    L.logInfo $ "balance1 (should be Just 10): " +|| balance1 ||+ "."
    -- Subtract 20
    balance2 <- unpack <$> makeRequestUnsafe connectCfg (BalanceChangeRequest (-20))
    L.logInfo $ "balance2 (should be Nothing): " +|| balance2 ||+ "."
    -- Add 101
    balance3 <- unpack <$> makeRequestUnsafe connectCfg (BalanceChangeRequest 101)
    L.logInfo $ "balance3 (should be Just 111): " +|| balance3 ||+ "."
    -- Final balance
    balance4 <- unpack <$> makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance4 (should be 111): " +|| balance4 ||+ "."

networkNode2 :: L.NodeDefinitionModel ()
networkNode2 = do
  L.nodeTag "networkNode2"
  L.scenario networkNode2Scenario

-- Scenario 3: 2 network nodes can interact (2)
-- One of them uses state to store some operational data.
-- It also holds a graph with transactions.
-- Other requests balance and amount change.

data NetworkNode3Data = NetworkNode3Data
  { _graphHeadVar :: D.StateVar D.StringHash
  , _balanceVar   :: D.StateVar Int
  }

makeLenses ''NetworkNode3Data

-- In this scenario, we assume the graph is list-like.
calculateBalance
  :: D.StringHash
  -> Int
  -> L.GraphModel Int
calculateBalance curNodeHash curBalance =
  L.getNode curNodeHash >>= \case
    Nothing -> error "Invalid reference found."
    Just curNode -> do
      let balanceChange = (D.fromContent $ curNode ^. Lens.content) ^. Lens.change
      case Map.toList (curNode ^. Lens.links) of
        [] -> pure $ curBalance + balanceChange
        [(nextNodeHash, _)] -> calculateBalanceTraversing nextNodeHash $ curBalance + balanceChange
        _ -> error "In this test scenario, graph should be list-like."

acceptGetBalance
  :: NetworkNode3Data
  -> GetBalanceRequest
  -> L.NodeModel GetBalanceResponse
acceptGetBalance nodeData GetBalanceRequest =
  GetBalanceResponse <$> (L.atomically $ L.readVar (nodeData ^. balanceVar))

acceptBalanceChange
  :: NetworkNode3Data
  -> BalanceChangeRequest
  -> L.NodeModel BalanceChangeResponse
acceptBalanceChange nodeData (BalanceChangeRequest change) = 
  L.atomically $ do
    curBalance   <- L.readVar $ nodeData ^. balanceVar
    graphHead    <- L.readVar $ nodeData ^. graphHeadVar
    mbNewBalance <- L.evalGraphStateF $ TG.tryAddTransaction' graphHead curBalance change
    case mbNewBalance of
      Nothing -> pure $ BalanceChangeResponse Nothing
      Just (newGraphHead, newBalance) -> do
        L.writeVar (nodeData ^. balanceVar) newBalance
        L.writeVar (nodeData ^. graphHeadVar) newGraphHead
        pure $ BalanceChangeResponse $ Just newBalance

newtorkNode3Initialization :: L.NodeModel NetworkNode3Data
newtorkNode3Initialization = do
  baseNode <- L.evalGraph $ L.getNode TG.nilTransactionHash >>= \case
    Nothing -> error "Graph is not ready: no genesis node found."
    Just baseNode -> pure baseNode
  balanceVar   <- L.atomically $ L.newVar 0
  graphHeadVar <- L.atomically $ L.newVar $ baseNode ^. Lens.hash
  pure $ NetworkNode3Data graphHeadVar balanceVar

networkNode3 :: L.NodeDefinitionModel ()
networkNode3 = do
  L.nodeTag "networkNode3"
  nodeData <- L.initialization newtorkNode3Initialization
  L.serving
    $ L.serve (acceptGetBalance nodeData)
    . L.serve (acceptBalanceChange nodeData)

networkNode4Scenario :: L.NodeModel ()
networkNode4Scenario = do
    let connectCfg = D.ConnectionConfig networkNode1Addr
    void $ makeRequestUnsafe connectCfg $ BalanceChangeRequest 10
    void $ makeRequestUnsafe connectCfg $ BalanceChangeRequest (-20)
    void $ makeRequestUnsafe connectCfg $ BalanceChangeRequest 101
    void $ makeRequestUnsafe connectCfg $ BalanceChangeRequest (-20)
    balance <- unpack <$> makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance (should be 91): " +|| balance ||+ "."

networkNode4 :: L.NodeDefinitionModel ()
networkNode4 = do
  L.nodeTag "networkNode4"
  L.scenario networkNode4Scenario
