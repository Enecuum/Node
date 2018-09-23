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


networkNode1Scenario :: L.NodeL ()
networkNode1Scenario = do
    let connectCfg = D.ConnectionConfig networkNode2Addr
    _ :: BalanceChangeResponse <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest 10
    _ :: BalanceChangeResponse <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest (-20)
    _ :: BalanceChangeResponse <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest 101
    _ :: BalanceChangeResponse <- makeRequestUnsafe' connectCfg $ BalanceChangeRequest (-20)
    GetBalanceResponse balance <- makeRequestUnsafe' connectCfg GetBalanceRequest
    L.logInfo $ "balance (should be 91): " +|| balance ||+ "."

networkNode1 :: L.NodeDefinitionL ()
networkNode1 = do
  L.nodeTag "networkNode1"
  L.scenario networkNode1Scenario
