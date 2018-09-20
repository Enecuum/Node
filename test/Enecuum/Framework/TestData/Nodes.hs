{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map

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
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.RpcMethod.Language 
import           Enecuum.Framework.Node.Language          ( NodeModel )

makeRpcRequest
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeModel (Either Text b)
makeRpcRequest connectCfg arg = L.evalNetworking $ L.makeRpcRequest connectCfg arg
            

makeRequestUnsafe
    :: (Typeable a, ToJSON a, FromJSON b) => D.ConnectionConfig -> a -> L.NodeModel b
makeRequestUnsafe connectCfg arg =
    (\(Right a) -> a) <$> makeRpcRequest connectCfg arg



bootNodeAddr, masterNode1Addr :: D.NodeAddress
bootNodeAddr = ConnectInfo "0.0.0.0" 1000
masterNode1Addr = ConnectInfo "0.0.0.1" 1000

networkNode1Addr, networkNode2Addr :: D.NodeAddress
networkNode1Addr = ConnectInfo "0.0.0.2" 1000
networkNode2Addr = ConnectInfo "0.0.0.3" 1000

bootNodeTag, masterNodeTag :: D.NodeTag
bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

-- | Boot node discovery sample scenario.
-- Currently, does nothing but returns the default boot node address.
simpleBootNodeDiscovery :: L.NetworkModel D.NodeAddress
simpleBootNodeDiscovery = pure bootNodeAddr

-- RPC handlers.

acceptHello1 :: HelloRequest1 ->  NodeModel HelloResponse1
acceptHello1 (HelloRequest1 msg) = pure $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: HelloRequest2 ->  NodeModel HelloResponse2
acceptHello2 (HelloRequest2 msg) = pure $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: GetHashIDRequest ->  NodeModel GetHashIDResponse
acceptGetHashId GetHashIDRequest = pure $ GetHashIDResponse "1"

acceptValidationRequest :: ValidationRequest -> L.NodeModel ValidationResponse
acceptValidationRequest req   = pure $ makeResponse $ verifyRequest req

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionModel ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.servingRpc 1000 $ do
    method acceptHello1
    method acceptGetHashId

masterNodeInitialization :: L.NodeModel (Either Text D.NodeID)
masterNodeInitialization = do
  addr     <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  Right (GetHashIDResponse eHashID)  <- makeRpcRequest (D.ConnectionConfig addr) GetHashIDRequest
  pure $ Right (D.NodeID eHashID)
  
masterNode :: L.NodeDefinitionModel ()
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
calculateBalance
  :: D.StringHash
  -> Int
  -> L.LGraphModel Int
calculateBalance curNodeHash curBalance = L.getNode curNodeHash >>= \case
  Nothing -> error "Invalid reference found."
  Just curNode -> do
    let trans = D.fromContent $ curNode ^. Lens.content
    let balanceChange = trans ^. Lens.change
    let links = curNode ^. Lens.links
    case Map.toList links of
      [] -> pure $ curBalance + balanceChange
      [(nextNodeHash, _)] -> calculateBalance nextNodeHash $ curBalance + balanceChange
      _ -> error "In this test scenario, graph should be list-like."

tryAddTransaction'
  :: (TNodeL D.Transaction)
  -> Int
  -> Int
  -> L.LGraphModel (Maybe (D.StringHash, Int))
tryAddTransaction' lastNode lastBalance change
  | lastBalance + change < 0 = pure Nothing
  | otherwise = do
      let newTrans = D.Transaction (lastNode ^. Lens.hash) change
      let newTransHash = D.toHash newTrans
      L.newNode newTrans
      L.newLink (lastNode ^. Lens.hash) newTransHash
      pure $ Just (lastNode ^. Lens.hash, lastBalance + change)

tryAddTransaction
  :: D.StringHash
  -> Int
  -> Int
  -> L.LGraphModel (Maybe (D.StringHash, Int))
tryAddTransaction curNodeHash prevBalance change = L.getNode curNodeHash >>= \case
  Nothing -> error "Invalid reference found."
  Just curNode -> do
    let trans = D.fromContent $ curNode ^. Lens.content
    let curBalanceChange = trans ^. Lens.change
    let curBalance = prevBalance + curBalanceChange
    let links = curNode ^. Lens.links
    case Map.toList links of
      [] -> tryAddTransaction' curNode curBalance change
      [(nextNodeHash, _)] -> tryAddTransaction nextNodeHash curBalance change
      _ -> error "In this test scenario, graph should be list-like."

acceptGetBalance :: TNodeL D.Transaction -> GetBalanceRequest -> NodeModel GetBalanceResponse
acceptGetBalance baseNode GetBalanceRequest = do
  balance <- L.evalGraph (calculateBalance (baseNode ^. Lens.hash) 0)
  pure $ GetBalanceResponse balance


acceptBalanceChange :: TNodeL D.Transaction -> BalanceChangeRequest -> NodeModel BalanceChangeResponse
acceptBalanceChange baseNode (BalanceChangeRequest change) = do
  mbHashAndBalance <- L.evalGraph $ tryAddTransaction (baseNode ^. Lens.hash) 0 change
  pure $ case mbHashAndBalance of
    Nothing                        -> BalanceChangeResponse Nothing
    Just (D.StringHash _, balance) -> BalanceChangeResponse (Just balance)

makeResult :: ToJSON a => Int -> a -> NodeModel RpcResponse
makeResult i a = pure $ RpcResponseResult (A.toJSON a) i

newtorkNode1Initialization :: L.NodeModel (TNodeL D.Transaction)
newtorkNode1Initialization = L.evalGraph $ TG.getTransactionNode TG.nilTransaction >>= \case
  Nothing -> error "Graph is not ready: no genesis node found."
  Just baseNode -> pure baseNode

networkNode1 :: L.NodeDefinitionModel ()
networkNode1 = do
  L.nodeTag "networkNode1"
  baseNode <- L.initialization newtorkNode1Initialization
  L.servingRpc 1000 $ do
    method (acceptGetBalance baseNode)
    method (acceptBalanceChange baseNode)



networkNode2Scenario :: L.NodeModel ()
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



networkNode2 :: L.NodeDefinitionModel ()
networkNode2 = do
  L.nodeTag "networkNode2"
  L.scenario networkNode2Scenario

  -- Scenario 3: boot node can validate data  recieved from master node

bootNodeValidation :: L.NodeDefinitionModel ()
bootNodeValidation = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.servingRpc 1000 $ do
      method acceptGetHashId
      method acceptValidationRequest

masterNodeInitializeWithValidation :: L.NodeModel (Either Text D.NodeID)
masterNodeInitializeWithValidation = do
  addr     <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  GetHashIDResponse eHashID  <- makeRequestUnsafe (D.ConnectionConfig addr) GetHashIDRequest
  validRes :: ValidationResponse <- makeRequestUnsafe (D.ConnectionConfig addr) ValidRequest
  L.logInfo $ "For the valid request recieved " +|| validRes ||+ "."
  invalidRes :: ValidationResponse <- makeRequestUnsafe (D.ConnectionConfig addr) InvalidRequest
  L.logInfo $ "For the invalid request recieved " +|| invalidRes ||+ "."
  pure $ Right (D.NodeID eHashID)

masterNodeValidation :: L.NodeDefinitionModel ()
masterNodeValidation = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitializeWithValidation
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
