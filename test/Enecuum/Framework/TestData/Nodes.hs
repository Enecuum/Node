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
import qualified Data.Text as Text


import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Framework.TestData.RPC
import           Enecuum.Framework.TestData.Validation
import qualified Enecuum.Framework.TestData.TestGraph as TG
import qualified Enecuum.Framework.Domain.Types as T
import           Enecuum.Legacy.Service.Network.Base (ConnectInfo (..))
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.RpcMethod.Language
import           Enecuum.Framework.Node.Language          ( NodeL )

makeRpcRequest
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeL (Either Text b)
makeRpcRequest connectCfg arg = L.evalNetworking $ L.makeRpcRequest connectCfg arg


makeRequestUnsafe
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeL b
makeRequestUnsafe connectCfg arg =
    (\(Right a) -> a) <$> makeRpcRequest connectCfg arg



bootNodeAddr, masterNode1Addr :: D.NodeAddress
bootNodeAddr = ConnectInfo "0.0.0.0" 1000
masterNode1Addr = ConnectInfo "0.0.0.1" 1000

networkNode1Addr, networkNode2Addr :: D.NodeAddress
networkNode1Addr = ConnectInfo "0.0.0.2" 1000
networkNode2Addr = ConnectInfo "0.0.0.3" 1000

networkNode3Addr, networkNode4Addr :: D.NodeAddress
networkNode3Addr = ConnectInfo "0.0.0.4" 1000
networkNode4Addr = ConnectInfo "0.0.0.5" 1000

bootNodeTag, masterNodeTag :: D.NodeTag
bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

-- | Boot node discovery sample scenario.
-- Currently, does nothing but returns the default boot node address.
simpleBootNodeDiscovery :: L.NetworkL D.NodeAddress
simpleBootNodeDiscovery = pure bootNodeAddr

-- RPC handlers.

acceptHello1 :: HelloRequest1 ->  NodeL HelloResponse1
acceptHello1 (HelloRequest1 msg) = pure $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: HelloRequest2 ->  NodeL HelloResponse2
acceptHello2 (HelloRequest2 msg) = pure $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: GetHashIDRequest ->  NodeL GetHashIDResponse
acceptGetHashId GetHashIDRequest = pure $ GetHashIDResponse "1"

acceptValidationRequest :: ValidationRequest -> L.NodeL ValidationResponse
acceptValidationRequest req   = pure $ makeResponse $ verifyRequest req

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionL ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.servingRpc 1000 $ do
    method acceptHello1
    method acceptGetHashId

masterNodeInitialization :: L.NodeL (Either Text D.NodeID)
masterNodeInitialization = do
  addr     <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  Right (GetHashIDResponse eHashID)  <- makeRpcRequest (D.ConnectionConfig addr) GetHashIDRequest
  pure $ Right (D.NodeID eHashID)

masterNode :: L.NodeDefinitionL ()
masterNode = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
  L.servingRpc 1000 $ do
    method acceptHello1
    method acceptHello2

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
  -> L.NodeL GetBalanceResponse
acceptGetBalanceTraversing baseNode GetBalanceRequest = do
  balance <- L.evalGraphIO (calculateBalanceTraversing (baseNode ^. Lens.hash) 0)
  pure $ GetBalanceResponse balance

acceptBalanceChangeTraversing
  :: TNodeL D.Transaction
  -> BalanceChangeRequest
  -> L.NodeL BalanceChangeResponse
acceptBalanceChangeTraversing baseNode (BalanceChangeRequest change) = do
  mbHashAndBalance <- L.evalGraphIO $ tryAddTransactionTraversing (baseNode ^. Lens.hash) 0 change
  case mbHashAndBalance of
    Nothing -> pure $ BalanceChangeResponse Nothing
    Just (D.StringHash _, balance) -> pure $ BalanceChangeResponse $ Just balance

newtorkNode1Initialization :: L.NodeL (TNodeL D.Transaction)
newtorkNode1Initialization = L.evalGraphIO $ L.getNode TG.nilTransactionHash >>= \case
  Nothing -> error "Graph is not ready: no genesis node found."
  Just baseNode -> pure baseNode

networkNode1 :: L.NodeDefinitionL ()
networkNode1 = do
  L.nodeTag "networkNode1"
  baseNode <- L.initialization newtorkNode1Initialization
  L.servingRpc 1000 $ do
    method (acceptGetBalanceTraversing baseNode)
    method (acceptBalanceChangeTraversing baseNode)

networkNode2Scenario :: L.NodeL ()
networkNode2Scenario = do
    let connectCfg = D.ConnectionConfig networkNode1Addr
    -- No balance change
    GetBalanceResponse balance0 <- makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance0 (should be 0): " +|| balance0 ||+ "."
    -- Add 10
    BalanceChangeResponse balance1 <- makeRequestUnsafe connectCfg $ BalanceChangeRequest 10
    L.logInfo $ "balance1 (should be Just 10): " +|| balance1 ||+ "."
    -- Subtract 20
    BalanceChangeResponse balance2 <- makeRequestUnsafe connectCfg $ BalanceChangeRequest (-20)
    L.logInfo $ "balance2 (should be Nothing): " +|| balance2 ||+ "."
    -- Add 101
    BalanceChangeResponse balance3 <- makeRequestUnsafe connectCfg $ BalanceChangeRequest 101
    L.logInfo $ "balance3 (should be Just 111): " +|| balance3 ||+ "."
    -- Final balance
    GetBalanceResponse balance4 <- makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance4 (should be 111): " +|| balance4 ||+ "."

networkNode2 :: L.NodeDefinitionL ()
networkNode2 = do
  L.nodeTag "networkNode2"
  L.scenario networkNode2Scenario

  -- Scenario 3: boot node can validate data  recieved from master node

bootNodeValidation :: L.NodeDefinitionL ()
bootNodeValidation = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.servingRpc 1000 $ do
      method acceptGetHashId
      method acceptValidationRequest

masterNodeInitializeWithValidation :: L.NodeL (Either Text D.NodeID)
masterNodeInitializeWithValidation = do
  addr     <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  GetHashIDResponse eHashID  <- makeRequestUnsafe (D.ConnectionConfig addr) GetHashIDRequest
  validRes :: ValidationResponse <- makeRequestUnsafe (D.ConnectionConfig addr) ValidRequest
  L.logInfo $ "For the valid request recieved " +|| validRes ||+ "."
  invalidRes :: ValidationResponse <- makeRequestUnsafe (D.ConnectionConfig addr) InvalidRequest
  L.logInfo $ "For the invalid request recieved " +|| invalidRes ||+ "."
  pure $ Right (D.NodeID eHashID)

masterNodeValidation :: L.NodeDefinitionL ()
masterNodeValidation = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitializeWithValidation
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."

-- Scenario 4: 2 network nodes can interact (2)
-- One of them uses state to store some operational data.
-- It also holds a graph with transactions.
-- Other requests balance and amount change.

data NetworkNode3Data = NetworkNode3Data
  { _graphHeadVar :: D.StateVar D.StringHash
  , _balanceVar   :: D.StateVar Int
  }

makeLenses ''NetworkNode3Data

acceptGetBalance
  :: NetworkNode3Data
  -> GetBalanceRequest
  -> L.NodeL GetBalanceResponse
acceptGetBalance nodeData GetBalanceRequest =
  GetBalanceResponse <$> (L.atomically $ L.readVar (nodeData ^. balanceVar))

acceptBalanceChange
  :: NetworkNode3Data
  -> BalanceChangeRequest
  -> L.NodeL BalanceChangeResponse
acceptBalanceChange nodeData (BalanceChangeRequest change) =
  L.atomically $ do
    curBalance   <- L.readVar $ nodeData ^. balanceVar
    graphHead    <- L.readVar $ nodeData ^. graphHeadVar
    mbNewBalance <- L.evalGraph $ TG.tryAddTransaction' graphHead curBalance change
    case mbNewBalance of
      Nothing -> pure $ BalanceChangeResponse Nothing
      Just (newGraphHead, newBalance) -> do
        L.writeVar (nodeData ^. balanceVar) newBalance
        L.writeVar (nodeData ^. graphHeadVar) newGraphHead
        pure $ BalanceChangeResponse $ Just newBalance

newtorkNode3Initialization :: L.NodeL NetworkNode3Data
newtorkNode3Initialization = do
  baseNode <- L.evalGraphIO $ L.getNode TG.nilTransactionHash >>= \case
    Nothing -> error "Graph is not ready: no genesis node found."
    Just baseNode -> pure baseNode
  balanceVar   <- L.atomically $ L.newVar 0
  graphHeadVar <- L.atomically $ L.newVar $ baseNode ^. Lens.hash
  pure $ NetworkNode3Data graphHeadVar balanceVar

networkNode3 :: L.NodeDefinitionL ()
networkNode3 = do
  L.nodeTag "networkNode3"
  nodeData <- L.initialization newtorkNode3Initialization
  L.servingRpc 1000 $ do
    L.method (acceptGetBalance nodeData)
    L.method (acceptBalanceChange nodeData)

networkNode4Scenario :: L.NodeL ()
networkNode4Scenario = do
    let connectCfg = D.ConnectionConfig networkNode3Addr
    _ :: BalanceChangeResponse <- makeRequestUnsafe connectCfg $ BalanceChangeRequest 10
    _ :: BalanceChangeResponse <- makeRequestUnsafe connectCfg $ BalanceChangeRequest (-20)
    _ :: BalanceChangeResponse <- makeRequestUnsafe connectCfg $ BalanceChangeRequest 101
    _ :: BalanceChangeResponse <- makeRequestUnsafe connectCfg $ BalanceChangeRequest (-20)
    GetBalanceResponse balance <- makeRequestUnsafe connectCfg GetBalanceRequest
    L.logInfo $ "balance (should be 91): " +|| balance ||+ "."

networkNode4 :: L.NodeDefinitionL ()
networkNode4 = do
  L.nodeTag "networkNode4"
  L.scenario networkNode4Scenario
