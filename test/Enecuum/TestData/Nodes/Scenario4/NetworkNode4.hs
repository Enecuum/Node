{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.TestData.Nodes.Scenario4.NetworkNode4 where

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

-- Scenario 4: 2 network nodes can interact (2)
-- One of them uses state to store some operational data.
-- It also holds a graph with transactions.
-- Other requests balance and amount change.

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
