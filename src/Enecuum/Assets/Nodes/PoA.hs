{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoA where

import qualified Data.Text as T

import           Data.HGraph.StringHashable   (toHash)
import           Enecuum.Assets.Nodes.Address (poaNodeAddress, graphNodeRpcAddress, graphNodeUdpAddress)
import qualified Enecuum.Domain               as D
import qualified Enecuum.Language             as L
import qualified Enecuum.Blockchain.Lens      as Lens
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))

data PoANodeData = PoANodeData
    { _currentLastKeyBlock :: D.StateVar D.KBlock
    , _status              :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''PoANodeData

showTransaction :: D.Transaction -> Text -> Text
showTransaction tx t =
    t <> ("\n    Tx: [" +|| tx ^.  Lens.owner ||+ "] -> [" +|| tx ^.  Lens.receiver ||+
          "], amount: " +|| tx ^.  Lens.amount ||+ ".")

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr showTransaction "" $ mBlock ^. Lens.transactions

poaNode :: L.NodeDefinitionL ()
poaNode = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    poaData <- L.scenario $ L.atomically (PoANodeData <$> L.newVar D.genesisKBlock <*> L.newVar NodeActing)

    L.std $ L.stdHandler $ L.stopNodeHandler poaData

    L.process $ forever $ do
        L.delay $ 100 * 1000
        eKBlock <- L.makeRpcRequest graphNodeRpcAddress GetLastKBlock
        case eKBlock of
            Left  _     -> pure ()
            Right block -> do
                currentBlock <- L.atomically $ L.readVar (poaData ^. currentLastKeyBlock)
                when (block /= currentBlock) $ do
                    L.logInfo $ "Empty KBlock found (" +|| toHash block ||+ "). Generating transactions & MBlock..."
                    L.atomically $ L.writeVar (poaData ^. currentLastKeyBlock) block
                    mBlock <- D.genRandMicroblock block
                    L.logInfo
                        $ "MBlock generated (" +|| toHash mBlock ||+ ". Transactions:" +| showTransactions mBlock |+ ""
                    L.send graphNodeUdpAddress mBlock

    L.awaitNodeFinished poaData
