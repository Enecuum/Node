{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- TODO: this is copy-paste from tests with little changes.
module Enecuum.Assets.Nodes.NetworkNode1 where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import qualified Data.Text as Text
import           Control.Lens                  (makeFieldsNoPrefix)

import           Enecuum.Config                (Config)
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens

import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.RpcMethod.Language
import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address


networkNode1Scenario :: L.NodeL ()
networkNode1Scenario = do
    L.logInfo $ "Network node 1: requests balance."
    GetBalanceResponse balance0 <- L.makeRpcRequestUnsafe networkNode2Addr GetBalanceRequest
    L.logInfo $ "Network node 1: Balance (should be 0): " +|| balance0 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (+10)."
    BalanceChangeResponse balance1 <- L.makeRpcRequestUnsafe networkNode2Addr $ BalanceChangeRequest 10
    L.logInfo $ "Network node 1: Balance: " +|| balance1 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (-20)."
    BalanceChangeResponse balance2 <- L.makeRpcRequestUnsafe networkNode2Addr $ BalanceChangeRequest (-20)
    L.logInfo $ "Network node 1: Balance: " +|| balance2 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (+101)."
    BalanceChangeResponse balance3 <- L.makeRpcRequestUnsafe networkNode2Addr $ BalanceChangeRequest 101
    L.logInfo $ "Network node 1: Balance: " +|| balance3 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (-20)."
    BalanceChangeResponse balance4 <- L.makeRpcRequestUnsafe networkNode2Addr $ BalanceChangeRequest (-20)
    L.logInfo $ "Network node 1: Balance: " +|| balance4 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (+10)."
    GetBalanceResponse balance <- L.makeRpcRequestUnsafe networkNode2Addr GetBalanceRequest
    L.logInfo $ "Network node 1: Balance (should be 91): " +|| balance ||+ "."
    L.stopNode

networkNode1 :: L.NodeDefinitionL ()
networkNode1 = do
  L.nodeTag "networkNode1"
  L.scenario networkNode1Scenario
