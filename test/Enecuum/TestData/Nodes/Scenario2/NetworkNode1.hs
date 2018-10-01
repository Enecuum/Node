{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.TestData.Nodes.Scenario2.NetworkNode1 where

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
import           Enecuum.TestData.Validation
import qualified Enecuum.TestData.TestGraph as TG
import           Enecuum.TestData.Nodes.Address

-- Scenario 2: 2 network nodes can interact.
-- One holds a graph with transactions. Other requests balance and amount change.

data NetworkNode1Data = NetworkNode1Data
  { _graph    :: TG.TestGraphVar
  , _baseNode :: T.TNodeL D.Transaction
  }

makeFieldsNoPrefix ''NetworkNode1Data

-- In this scenario, we assume the graph is list-like.
calculateBalanceTraversing
  :: D.StringHash
  -> D.Balance
  -> TG.TestGraphL D.Balance
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
  -> D.Balance
  -> D.BalanceChange
  -> TG.TestGraphL (Maybe (D.StringHash, D.Balance))
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
  :: NetworkNode1Data
  -> GetBalanceRequest
  -> L.NodeL GetBalanceResponse
acceptGetBalanceTraversing nodeData GetBalanceRequest = do
  balance <- L.withGraphIO nodeData
      $ calculateBalanceTraversing (nodeData ^. baseNode . Lens.hash) 0
  pure $ GetBalanceResponse balance

acceptBalanceChangeTraversing
  :: NetworkNode1Data
  -> BalanceChangeRequest
  -> L.NodeL BalanceChangeResponse
acceptBalanceChangeTraversing nodeData (BalanceChangeRequest change) = do
  mbHashAndBalance <- L.withGraphIO nodeData
      $ tryAddTransactionTraversing (nodeData ^. baseNode . Lens.hash) 0 change
  case mbHashAndBalance of
    Nothing -> pure $ BalanceChangeResponse Nothing
    Just (D.StringHash _, balance) -> pure $ BalanceChangeResponse $ Just balance

newtorkNode1Initialization :: TG.TestGraphVar -> L.NodeL NetworkNode1Data
newtorkNode1Initialization g =
  L.evalGraphIO g $ L.getNode TG.nilTransactionHash >>= \case
    Nothing -> error "Graph is not ready: no genesis node found."
    Just baseNode -> pure $ NetworkNode1Data g baseNode

networkNode1 :: TG.TestGraphVar -> L.NodeDefinitionL ()
networkNode1 g = do
  L.nodeTag "networkNode1"
  nodeData <- L.initialization $ newtorkNode1Initialization g
  L.servingRpc 2000 $ do
    L.method (acceptGetBalanceTraversing nodeData)
    L.method (acceptBalanceChangeTraversing nodeData)
