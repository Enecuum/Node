{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- TODO: this is copy-paste from tests with little changes.
module Enecuum.Assets.Nodes.NetworkNode1 where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import           Control.Lens                  (makeFieldsNoPrefix)

import           Enecuum.Config                (Config)
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import qualified Data.Text as Text

import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Legacy.Service.Network.Base (ConnectInfo (..))
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.RpcMethod.Language
import           Enecuum.Framework.Node.Language          ( NodeL )
import           Enecuum.Blockchain.Language.Extra
import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address


networkNode1Scenario :: L.NodeL cfg ()
networkNode1Scenario = do
    let connectCfg = D.ConnectionConfig networkNode2Addr

    L.logInfo $ "Network node 1: requests balance."
    GetBalanceResponse balance0 <- makeRequestUnsafe' connectCfg GetBalanceRequest
    L.logInfo $ "Network node 1: Balance (should be 0): " +|| balance0 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (+10)."
    BalanceChangeResponse balance1 <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest 10
    L.logInfo $ "Network node 1: Balance: " +|| balance1 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (-20)."
    BalanceChangeResponse balance2 <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest (-20)
    L.logInfo $ "Network node 1: Balance: " +|| balance2 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (+101)."
    BalanceChangeResponse balance3 <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest 101
    L.logInfo $ "Network node 1: Balance: " +|| balance3 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (-20)."
    BalanceChangeResponse balance4 <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest (-20)
    L.logInfo $ "Network node 1: Balance: " +|| balance4 ||+ "."

    L.logInfo $ "Network node 1: requests balance change (+10)."
    GetBalanceResponse balance <- makeRequestUnsafe' connectCfg GetBalanceRequest
    L.logInfo $ "Network node 1: Balance (should be 91): " +|| balance ||+ "."

networkNode1 :: L.NodeDefinitionL cfg ()
networkNode1 = do
  L.nodeTag "networkNode1"
  L.scenario networkNode1Scenario
