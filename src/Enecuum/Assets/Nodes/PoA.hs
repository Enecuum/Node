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
    , _transactionPending  :: D.StateVar [D.Transaction]    
    }

makeFieldsNoPrefix ''PoANodeData

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr D.showTransaction "" $ mBlock ^. Lens.transactions

poaNode :: D.ScenarioRole -> L.NodeDefinitionL ()
poaNode role = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    poaData <- L.scenario $ L.atomically (PoANodeData <$> L.newVar D.genesisKBlock <*> L.newVar NodeActing <*> L.newVar [])

    L.std $ L.stdHandler $ L.stopNodeHandler poaData
    L.process $ forever $ do
        L.delay $ 100 * 1000
        whenRightM (L.makeRpcRequest graphNodeTransmitterRpcAddress GetTransactionPending) $ \tx -> do
            forM_ tx (\t -> L.logInfo $ "\nAdd transaction to pending "  +| D.showTransaction t "" |+ "")
            L.atomically $ L.modifyVar (poaData ^. transactionPending) ( ++ tx )       
        whenRightM (L.makeRpcRequest graphNodeTransmitterRpcAddress GetLastKBlock) $ \block -> do
            currentBlock <- L.atomically $ L.readVar (poaData ^. currentLastKeyBlock)        
            when (block /= currentBlock) $ do
                L.logInfo $ "Empty KBlock found (" +|| toHash block ||+ ")."

                pendingTransactions <- L.atomically $ do
                    tx <- (take A.transactionsInMicroblock) <$> L.readVar (poaData ^. transactionPending)
                    L.modifyVar (poaData ^. transactionPending) (drop $ length tx)
                    pure tx

                let pendingTransactionsCount = length pendingTransactions
                let transactionsCount = A.transactionsInMicroblock - pendingTransactionsCount

                when (pendingTransactionsCount > 0) $ L.logInfo $ "\nGet " +|| pendingTransactionsCount ||+ " transaction(s) from pending " 
                when (transactionsCount        > 0) $ L.logInfo $ "Generate "  +|| transactionsCount ||+ " random transaction(s)."

                txGenerated <- replicateM transactionsCount $ A.genTransaction A.Hardcoded

                let tx = pendingTransactions ++ txGenerated 

                L.atomically $ L.writeVar (poaData ^. currentLastKeyBlock) block
                mBlock <- case role of
                    D.Good -> A.genMicroblock block tx
                    D.Bad  -> A.generateBogusSignedMicroblock block tx
                L.logInfo
                    $ "MBlock generated (" +|| toHash mBlock ||+ ". Transactions:" +| showTransactions mBlock |+ ""
                void $ L.withConnection D.Tcp graphNodeTransmitterTcpAddress $
                    \conn -> L.send conn mBlock


    L.awaitNodeFinished poaData
