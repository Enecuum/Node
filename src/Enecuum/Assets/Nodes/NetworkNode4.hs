{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- TODO: this is copy-paste from tests with little changes.
module Enecuum.Assets.Nodes.NetworkNode4 where

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
import Enecuum.Assets.Nodes.NetworkNode3 (NetworkNodeChainData(..), chainLengthVar)


checkLengthAndUpdate connectCfg nodeData = do
    L.logInfo $ "Network node 4: requests chain length."
    GetChainLengthResponse newLength <- makeRequestUnsafe' connectCfg GetChainLengthRequest
    L.logInfo $ "Network node 4: Network node 3 has Chain (should be 15): " +|| newLength ||+ "."

    L.logInfo $ "Network node 4: update chain length if it's bigger."
    L.atomically $ do
      curChain  <- L.readVar $ nodeData ^. chainLengthVar
      when (curChain < newLength) $ 
            L.writeVar (nodeData ^. chainLengthVar) newLength

--networkNode4Scenario :: L.NodeL ()
networkNode4Scenario nodeData = do
    let connectCfg = D.ConnectionConfig networkNode3Addr

    L.logInfo $ "Network node 4: check current chain length."
    length0 <- L.atomically $ L.readVar (nodeData ^. chainLengthVar)
    L.logInfo $ "Network node 4: Chain length (should be 5): " +|| length0 ||+ "."

    checkLengthAndUpdate connectCfg nodeData

    L.logInfo $ "Network node 4: check current chain length."
    length1 <- L.atomically $ L.readVar (nodeData ^. chainLengthVar)
    L.logInfo $ "Network node 4: Chain length (should be 15): " +|| length1 ||+ "."

newtorkNode4Initialization :: L.NodeL NetworkNodeChainData
newtorkNode4Initialization = do
  chainLengthVar'   <- L.atomically $ L.newVar 5
  pure $ NetworkNodeChainData chainLengthVar'

networkNode4 :: L.NodeDefinitionL ()
networkNode4 = do
  L.nodeTag "networkNode4"
  nodeData <- L.initialization $ newtorkNode4Initialization
  L.scenario $ networkNode4Scenario nodeData
