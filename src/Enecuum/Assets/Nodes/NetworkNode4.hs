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
import Enecuum.Assets.Nodes.Types


checkLengthAndUpdate connectCfg nodeData = do
    L.logInfo $ "Network node 4: requests chain length."
    GetChainLengthResponse otherLength <- makeRequestUnsafe' connectCfg GetChainLengthRequest
    L.logInfo $ "Network node 4: Network node 3 has Chain (should be 15): " +|| otherLength ||+ "."

    L.logInfo $ "Network node 4: update chain if it's bigger."
    curChain <- L.atomically <$> L.readVar $ nodeData ^. chainVar
    let curChainLength = length curChain
    when (curChainLength < otherLength) $ do
      GetChainFromResponse chainTail <- makeRequestUnsafe' connectCfg (GetChainFromRequest curChainLength)
      L.atomically $ do
        updChain <- L.readVar $ nodeData ^. chainVar
        let restChain = drop (length updChain - curChainLength) chainTail
        L.writeVar (nodeData ^. chainVar) (curChain ++ restChain)

--networkNode4Scenario :: L.NodeL ()
networkNode4Scenario nodeData = do
    let connectCfg = D.ConnectionConfig networkNode3Addr

    L.logInfo $ "Network node 4: check current chain."
    chain0 <- L.atomically $ L.readVar (nodeData ^. chainVar)
    L.logInfo $ "Network node 4: Chain (should has length 5): " +|| chain0 ||+ "."

    checkLengthAndUpdate connectCfg nodeData

    L.logInfo $ "Network node 4: check current chain."
    chain1 <- L.atomically $ L.readVar (nodeData ^. chainVar)
    L.logInfo $ "Network node 4: Chain (should has length 15): " +|| chain1 ||+ "."

newtorkNode4Initialization :: L.NodeL NetworkNodeChainData
newtorkNode4Initialization = do
  chainLengthVar'   <- L.atomically $ L.newVar $ map D.Block [0..4]
  pure $ NetworkNodeChainData chainLengthVar'

networkNode4 :: L.NodeDefinitionL ()
networkNode4 = do
  L.nodeTag "networkNode4"
  nodeData <- L.initialization $ newtorkNode4Initialization
  L.scenario $ networkNode4Scenario nodeData
