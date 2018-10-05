{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

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

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Types


-- checkLengthAndUpdate
--   :: HasChainVar s (D.StateVar [D.KBlock]) =>
--      D.Address -> s -> Free L.NodeF ()
checkLengthAndUpdate address nodeData = do
    L.logInfo $ "Network node 4: requests chain length."
    GetChainLengthResponse otherLength <- L.makeRpcRequestUnsafe address GetChainLengthRequest
    L.logInfo $ "Network node 4: Network node 3 has Chain (should be 15): " +|| otherLength ||+ "."

    L.logInfo $ "Network node 4: update chain if it's bigger."
    curChain <- L.atomically <$> L.readVar $ nodeData ^. chainVar
    let curChainLength = length curChain
    when (curChainLength < otherLength) $ do
      GetChainFromResponse chainTail <- L.makeRpcRequestUnsafe address (GetChainFromRequest curChainLength )
      logs <- L.atomically $ do
        updChain <- L.readVar $ nodeData ^. chainVar
        let l1 = "updChain: " :: Text.Text
        let l2 = (show updChain :: Text.Text)
        let restChain = chainTail
        let l3 = "restChain: " :: Text.Text
        let l4 = (show restChain :: Text.Text)
        L.writeVar (nodeData ^. chainVar) (curChain ++ restChain)
        let messages =  (l1 : l2 : l3 : l4 : [] ) 
        pure messages  
      forM_ logs L.logInfo

--networkNode4Scenario :: L.NodeL ()
networkNode4Scenario nodeData = do
    L.logInfo $ "Network node 4: check current chain."
    chain0 <- L.atomically $ L.readVar (nodeData ^. chainVar)
    L.logInfo $ "Network node 4: Chain (should has length 5): " +|| chain0 ||+ "."

    checkLengthAndUpdate networkNode3Addr nodeData

    L.logInfo $ "Network node 4: check current chain."
    chain1 <- L.atomically $ L.readVar (nodeData ^. chainVar)
    L.logInfo $ "Network node 4: Chain (should has length 15): " +|| chain1 ||+ "."

newtorkNode4Initialization :: L.NodeL NetworkNodeChainData
newtorkNode4Initialization = do
  (_, blocks) <- D.generateNKBlocks 5
  chainLengthVar' <- L.atomically $ L.newVar blocks 
  pure $ NetworkNodeChainData chainLengthVar'

networkNode4 :: L.NodeDefinitionL ()
networkNode4 = do
  L.nodeTag "networkNode4"
  nodeData <- L.initialization $ newtorkNode4Initialization
  L.scenario $ networkNode4Scenario nodeData
