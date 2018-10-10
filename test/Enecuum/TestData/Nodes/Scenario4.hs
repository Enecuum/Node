{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.TestData.Nodes.Scenario4 where

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
    GetBalanceResponse <$> (L.atomically $ L.readVar (nodeData ^. balanceVar))

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
    baseNode <- L.evalGraphIO g $ L.getNode TG.nilTransactionHash >>= \case
        Nothing       -> error "Graph is not ready: no genesis node found."
        Just baseNode -> pure baseNode
    balanceVar   <- L.atomically $ L.newVar 0
    graphHeadVar <- L.atomically $ L.newVar $ baseNode ^. Lens.hash
    pure $ NetworkNode3Data g graphHeadVar balanceVar

networkNode3 :: TG.TestGraphVar -> L.NodeDefinitionL ()
networkNode3 g = do
    L.nodeTag "networkNode3"
    nodeData <- L.initialization $ newtorkNode3Initialization g
    L.serving 2000 $ do
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
