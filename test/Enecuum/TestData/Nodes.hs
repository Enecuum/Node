{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Map                      as Map
import           Control.Lens                  (makeFieldsNoPrefix)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Core.Lens             as Lens
import           Enecuum.Language              (HasGraph)

import qualified Enecuum.Core.HGraph.Internal.Types as T

import           Enecuum.TestData.RPC
import           Enecuum.TestData.Validation
import qualified Enecuum.TestData.TestGraph as TG

bootNodeAddr, masterNode1Addr :: D.Address
bootNodeAddr = D.Address "0.0.0.0" 2000
masterNode1Addr = D.Address "0.0.0.1" 2000

networkNode1Addr, networkNode2Addr :: D.Address
networkNode1Addr = D.Address "0.0.0.2" 2000
networkNode2Addr = D.Address "0.0.0.3" 2000

networkNode3Addr, networkNode4Addr :: D.Address
networkNode3Addr = D.Address "0.0.0.4" 2000
networkNode4Addr = D.Address "0.0.0.5" 2000

bootNodeTag, masterNodeTag :: D.NodeTag
bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

-- | Boot node discovery sample scenario.
-- Currently, does nothing but returns the default boot node address.
simpleBootNodeDiscovery :: L.NodeL D.Address
simpleBootNodeDiscovery = pure bootNodeAddr

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionL ()
bootNode = do
    L.nodeTag bootNodeTag
    void $ L.initialization $ pure $ D.NodeID "abc"
    void $ L.serving D.Rpc 2000 $ do
        L.method acceptHello1
        L.method acceptGetHashId

masterNodeInitialization :: L.NodeL (Either Text D.NodeID)
masterNodeInitialization = do
    addr                      <- simpleBootNodeDiscovery
    GetHashIDResponse eHashID <- L.makeRpcRequestUnsafe addr GetHashIDRequest
    pure $ Right (D.NodeID eHashID)

masterNode :: L.NodeDefinitionL ()
masterNode = do
    L.nodeTag masterNodeTag
    nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
    L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
    void $ L.serving D.Rpc 2000 $ do
        L.method acceptHello1
        L.method acceptHello2

-- Scenario 2: 2 network nodes can interact.
-- One holds a graph with transactions. Other requests balance and amount change.

data NetworkNode1Data = NetworkNode1Data
  { _graph    :: TG.TestGraphVar
  , _baseNode :: T.TNodeL TG.Transaction
  }

makeFieldsNoPrefix ''NetworkNode1Data

-- In this scenario, we assume the graph is list-like.
calculateBalanceTraversing :: D.StringHash -> TG.Balance -> TG.TestGraphL TG.Balance
calculateBalanceTraversing curNodeHash curBalance = L.getNode curNodeHash >>= \case
    Nothing      -> error "Invalid reference found."
    Just curNode -> do
        let balanceChange = D.fromContent (curNode ^. Lens.content) ^. TG.change
        case Map.toList (curNode ^. Lens.links) of
            []                  -> pure $ curBalance + balanceChange
            [(nextNodeHash, _)] -> calculateBalanceTraversing nextNodeHash $ curBalance + balanceChange
            _                   -> error "In this test scenario, graph should be list-like."

tryAddTransactionTraversing
    :: D.StringHash -> TG.Balance -> TG.BalanceChange -> TG.TestGraphL (Maybe (D.StringHash, TG.Balance))
tryAddTransactionTraversing curNodeHash prevBalance change = L.getNode curNodeHash >>= \case
    Nothing      -> error "Invalid reference found."
    Just curNode -> do
        let curBalanceChange = D.fromContent (curNode ^. Lens.content) ^. TG.change
        let curBalance       = prevBalance + curBalanceChange
        case Map.toList (curNode ^. Lens.links) of
            []                  -> TG.tryAddTransaction' (curNode ^. Lens.hash) curBalance change
            [(nextNodeHash, _)] -> tryAddTransactionTraversing nextNodeHash curBalance change
            _                   -> error "In this test scenario, graph should be list-like."

acceptGetBalanceTraversing :: NetworkNode1Data -> GetBalanceRequest -> L.NodeL GetBalanceResponse
acceptGetBalanceTraversing nodeData GetBalanceRequest = do
    balance <- L.withGraphIO nodeData $ calculateBalanceTraversing (nodeData ^. baseNode . Lens.hash) 0
    pure $ GetBalanceResponse balance

acceptBalanceChangeTraversing :: NetworkNode1Data -> BalanceChangeRequest -> L.NodeL BalanceChangeResponse
acceptBalanceChangeTraversing nodeData (BalanceChangeRequest change) = do
    mbHashAndBalance <- L.withGraphIO nodeData $ tryAddTransactionTraversing (nodeData ^. baseNode . Lens.hash) 0 change
    case mbHashAndBalance of
        Nothing                        -> pure $ BalanceChangeResponse Nothing
        Just (D.StringHash _, balance) -> pure $ BalanceChangeResponse $ Just balance

newtorkNode1Initialization :: TG.TestGraphVar -> L.NodeL NetworkNode1Data
newtorkNode1Initialization g = L.evalGraphIO g $ L.getNode TG.nilTransactionHash >>= \case
    Nothing   -> error "Graph is not ready: no genesis node found."
    Just node -> pure $ NetworkNode1Data g node

networkNode1 :: TG.TestGraphVar -> L.NodeDefinitionL ()
networkNode1 g = do
    L.nodeTag "networkNode1"
    nodeData <- L.initialization $ newtorkNode1Initialization g
    void $ L.serving D.Rpc 2000 $ do
        L.method $ acceptGetBalanceTraversing nodeData
        L.method $ acceptBalanceChangeTraversing nodeData

networkNode2Scenario :: L.NodeL ()
networkNode2Scenario = do
    -- No balance change
    GetBalanceResponse balance0 <- L.makeRpcRequestUnsafe networkNode1Addr GetBalanceRequest
    L.logInfo $ "balance0 (should be 0): " +|| balance0 ||+ "."
    -- Add 10
    BalanceChangeResponse balance1 <- L.makeRpcRequestUnsafe networkNode1Addr $ BalanceChangeRequest 10
    L.logInfo $ "balance1 (should be Just 10): " +|| balance1 ||+ "."
    -- Subtract 20
    BalanceChangeResponse balance2 <- L.makeRpcRequestUnsafe networkNode1Addr $ BalanceChangeRequest (-20)
    L.logInfo $ "balance2 (should be Nothing): " +|| balance2 ||+ "."
    -- Add 101
    BalanceChangeResponse balance3 <- L.makeRpcRequestUnsafe networkNode1Addr $ BalanceChangeRequest 101
    L.logInfo $ "balance3 (should be Just 111): " +|| balance3 ||+ "."
    -- Final balance
    GetBalanceResponse balance4 <- L.makeRpcRequestUnsafe networkNode1Addr GetBalanceRequest
    L.logInfo $ "balance4 (should be 111): " +|| balance4 ||+ "."

networkNode2 :: L.NodeDefinitionL ()
networkNode2 = do
    L.nodeTag "networkNode2"
    L.scenario networkNode2Scenario

  -- Scenario 3: boot node can validate data  recieved from master node

bootNodeValidation :: L.NodeDefinitionL ()
bootNodeValidation = do
    L.nodeTag bootNodeTag
    void $ L.initialization $ pure $ D.NodeID "abc"
    void $ L.serving D.Rpc 2000 $ do
        L.method acceptGetHashId
        L.method acceptValidationRequest

masterNodeInitializeWithValidation :: L.NodeL (Either Text D.NodeID)
masterNodeInitializeWithValidation = do
    addr                           <- simpleBootNodeDiscovery
    GetHashIDResponse eHashID      <- L.makeRpcRequestUnsafe addr GetHashIDRequest
    validRes :: ValidationResponse <- L.makeRpcRequestUnsafe addr ValidRequest
    L.logInfo $ "For the valid request recieved " +|| validRes ||+ "."
    invalidRes :: ValidationResponse <- L.makeRpcRequestUnsafe addr InvalidRequest
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
  { _graph        :: TG.TestGraphVar
  , _graphHeadVar :: D.StateVar D.StringHash
  , _balanceVar   :: D.StateVar Int
  }

makeFieldsNoPrefix ''NetworkNode3Data

acceptGetBalance :: NetworkNode3Data -> GetBalanceRequest -> L.NodeL GetBalanceResponse
acceptGetBalance nodeData GetBalanceRequest =
    GetBalanceResponse <$> L.atomically (L.readVar (nodeData ^. balanceVar))

acceptBalanceChange :: NetworkNode3Data -> BalanceChangeRequest -> L.NodeL BalanceChangeResponse
acceptBalanceChange nodeData (BalanceChangeRequest change) = L.atomically $ do
    curBalance   <- L.readVar $ nodeData ^. balanceVar
    graphHead    <- L.readVar $ nodeData ^. graphHeadVar
    mbNewBalance <- L.withGraph nodeData $ TG.tryAddTransaction' graphHead curBalance change
    case mbNewBalance of
        Nothing                         -> pure $ BalanceChangeResponse Nothing
        Just (newGraphHead, newBalance) -> do
            L.writeVar (nodeData ^. balanceVar)   newBalance
            L.writeVar (nodeData ^. graphHeadVar) newGraphHead
            pure $ BalanceChangeResponse $ Just newBalance

newtorkNode3Initialization :: TG.TestGraphVar -> L.NodeL NetworkNode3Data
newtorkNode3Initialization g = do
    node <- L.evalGraphIO g $ L.getNode TG.nilTransactionHash >>= \case
        Nothing   -> error "Graph is not ready: no genesis node found."
        Just node -> pure node
    balance   <- L.atomically $ L.newVar 0
    graphHead <- L.atomically $ L.newVar $ node ^. Lens.hash
    pure $ NetworkNode3Data g graphHead balance

networkNode3 :: TG.TestGraphVar -> L.NodeDefinitionL ()
networkNode3 g = do
    L.nodeTag "networkNode3"
    nodeData <- L.initialization $ newtorkNode3Initialization g
    void $ L.serving D.Rpc 2000 $ do
        L.method (acceptGetBalance nodeData)
        L.method (acceptBalanceChange nodeData)

networkNode4Scenario :: L.NodeL ()
networkNode4Scenario = do
    _ :: BalanceChangeResponse <- L.makeRpcRequestUnsafe networkNode3Addr $ BalanceChangeRequest 10
    _ :: BalanceChangeResponse <- L.makeRpcRequestUnsafe networkNode3Addr $ BalanceChangeRequest (-20)
    _ :: BalanceChangeResponse <- L.makeRpcRequestUnsafe networkNode3Addr $ BalanceChangeRequest 101
    _ :: BalanceChangeResponse <- L.makeRpcRequestUnsafe networkNode3Addr $ BalanceChangeRequest (-20)
    GetBalanceResponse balance <- L.makeRpcRequestUnsafe networkNode3Addr GetBalanceRequest
    L.logInfo $ "balance (should be 91): " +|| balance ||+ "."

networkNode4 :: L.NodeDefinitionL ()
networkNode4 = do
    L.nodeTag "networkNode4"
    L.scenario networkNode4Scenario
