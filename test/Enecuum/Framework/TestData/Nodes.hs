{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens

import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Framework.TestData.RPC
import qualified Enecuum.Framework.TestData.TestGraph as TG
import qualified Enecuum.Framework.Domain.Types as T
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.RpcMethod.Language 
import           Enecuum.Framework.Node.Language          ( NodeModel )

makeRequestSafe
  :: D.ConnectionConfig
  -> RpcRequest
  -> L.NodeModel (Either Text RpcResponse)
makeRequestSafe cfg = L.evalNetworking . L.withConnection cfg

makeRequestUnsafe
  :: D.ConnectionConfig
  -> RpcRequest
  -> L.NodeModel RpcResponse
makeRequestUnsafe cfg = D.withSuccess . L.evalNetworking . L.withConnection cfg

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

acceptHello1 :: A.Value -> Int ->  NodeModel RpcResponse
acceptHello1 (A.fromJSON -> A.Success (HelloRequest1 msg)) i = makeResult i $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: A.Value -> Int ->  NodeModel RpcResponse
acceptHello2 (A.fromJSON -> A.Success (HelloRequest2 msg)) i = makeResult i $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: A.Value -> Int ->  NodeModel RpcResponse
acceptGetHashId (A.fromJSON -> A.Success GetHashIDRequest) i = makeResult i $ GetHashIDResponse "1"

-- Scenario 1: master node can interact with boot node.

bootNode :: L.NodeDefinitionModel ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.servingRpc $ do
    rpcMethod "hello1"    acceptHello1
    rpcMethod "getHashId" acceptGetHashId

masterNodeInitialization :: L.NodeModel (Either Text D.NodeID)
masterNodeInitialization = do
  addr     <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  GetHashIDResponse eHashID  <- getUnsafe (D.ConnectionConfig addr) "getHashId" GetHashIDRequest
  pure $ Right (D.NodeID eHashID)
  
masterNode :: L.NodeDefinitionModel ()
masterNode = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
  L.servingRpc $ do
    rpcMethod "hello1" acceptHello1
    rpcMethod "hello2" acceptHello2

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

acceptGetBalance :: TNodeL D.Transaction -> A.Value -> Int -> NodeModel RpcResponse
acceptGetBalance baseNode (A.fromJSON -> A.Success GetBalanceRequest) i = do
  balance <- L.evalGraph (calculateBalance (baseNode ^. Lens.hash) 0)
  makeResult i $ GetBalanceResponse balance


acceptBalanceChange :: TNodeL D.Transaction -> A.Value -> Int -> NodeModel RpcResponse
acceptBalanceChange baseNode (A.fromJSON -> A.Success (BalanceChangeRequest change)) i = do
  mbHashAndBalance <- L.evalGraph $ tryAddTransaction (baseNode ^. Lens.hash) 0 change
  case mbHashAndBalance of
    Nothing                        -> makeResult i $ BalanceChangeResponse Nothing
    Just (D.StringHash _, balance) -> makeResult i $ BalanceChangeResponse $ Just balance


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
  L.servingRpc $ do
    rpcMethod "getBalance"    (acceptGetBalance baseNode)
    rpcMethod "balanceChange" (acceptBalanceChange baseNode)

networkNode2Scenario :: L.NodeModel ()
networkNode2Scenario = do
    let connectCfg = D.ConnectionConfig networkNode1Addr
    -- No balance change
    GetBalanceResponse balance0 <- getUnsafe connectCfg "getBalance" GetBalanceRequest
    L.logInfo $ "balance0 (should be 0): " +|| balance0 ||+ "."
    -- Add 10
    BalanceChangeResponse balance1 <- getUnsafe connectCfg "balanceChange" $ BalanceChangeRequest 10
    L.logInfo $ "balance1 (should be Just 10): " +|| balance1 ||+ "."
    -- Subtract 20
    BalanceChangeResponse balance2 <- getUnsafe connectCfg "balanceChange" $ BalanceChangeRequest (-20)
    L.logInfo $ "balance2 (should be Nothing): " +|| balance2 ||+ "."
    -- Add 101
    BalanceChangeResponse balance3 <- getUnsafe connectCfg "balanceChange" $ BalanceChangeRequest 101
    L.logInfo $ "balance3 (should be Just 111): " +|| balance3 ||+ "."
    -- Final balance
    GetBalanceResponse balance4 <- getUnsafe connectCfg "getBalance" GetBalanceRequest
    L.logInfo $ "balance4 (should be 111): " +|| balance4 ||+ "."

getUnsafe :: (ToJSON a, FromJSON b) => D.ConnectionConfig -> Text -> a -> Free L.NodeF b
getUnsafe connectCfg name arg = do
  A.Success res <- A.fromJSON . content <$> makeRequestUnsafe connectCfg (makeRequest name arg)
  return res

networkNode2 :: L.NodeDefinitionModel ()
networkNode2 = do
  L.nodeTag "networkNode2"
  L.scenario networkNode2Scenario
