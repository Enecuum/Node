{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoA where

import qualified Data.Text as T

import           Data.HGraph.StringHashable   (toHash)
import           Enecuum.Assets.Nodes.Address (poaNodeAddress, graphNodeTransmitterRpcAddress, graphNodeTransmitterTcpAddress)
import qualified Enecuum.Domain               as D
import qualified Enecuum.Language             as L
import qualified Enecuum.Blockchain.Lens      as Lens
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))
import qualified Enecuum.Assets.Blockchain.Generation as A

data PoANodeData = PoANodeData
    { _currentLastKeyBlock :: D.StateVar D.KBlock
    , _status              :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''PoANodeData

showTransaction :: D.Transaction -> Text -> Text
showTransaction tx t =
    t <> ("\n    Tx: [" +|| ( D.showPublicKey $ tx ^.  Lens.owner ) ||+ "] -> [" +|| (D.showPublicKey $ tx ^.  Lens.receiver) ||+
          "], amount: " +|| tx ^.  Lens.amount ||+ ".")

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr showTransaction "" $ mBlock ^. Lens.transactions

poaNode :: D.ScenarioRole -> L.NodeDefinitionL ()
poaNode role = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    poaData <- L.scenario $ L.atomically (PoANodeData <$> L.newVar D.genesisKBlock <*> L.newVar NodeActing)

    L.std $ L.stdHandler $ L.stopNodeHandler poaData
    L.process $ forever $ do
        L.delay $ 100 * 1000
        whenRightM (L.makeRpcRequest graphNodeTransmitterRpcAddress GetLastKBlock) $ \block -> do
            currentBlock <- L.atomically $ L.readVar (poaData ^. currentLastKeyBlock)
            when (block /= currentBlock) $ do
                L.logInfo $ "Empty KBlock found (" +|| toHash block ||+ "). Generating transactions & MBlock..."
                L.atomically $ L.writeVar (poaData ^. currentLastKeyBlock) block
                mBlock <- case role of
                    D.Good -> A.genRandMicroblock block
                    D.Bad  -> A.generateBogusSignedMicroblock block
                L.logInfo
                    $ "MBlock generated (" +|| toHash mBlock ||+ ". Transactions:" +| showTransactions mBlock |+ ""

                void $ L.withConnection D.Tcp graphNodeTransmitterTcpAddress $
                    \conn -> L.send conn mBlock


    L.awaitNodeFinished poaData
