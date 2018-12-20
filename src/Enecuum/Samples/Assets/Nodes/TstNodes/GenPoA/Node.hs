{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Samples.Assets.Nodes.TstNodes.GenPoA.Node where

import qualified Data.Aeson                                  as A
import           Enecuum.Prelude

import           Data.HGraph.StringHashable                  (toHash)

import qualified Enecuum.Samples.Blockchain.Lens                     as Lens
import           Enecuum.Config
import qualified Enecuum.Domain                              as D
import           Enecuum.Framework.Language.Extra            (HasStatus)
import qualified Enecuum.Framework.Lens                      as Lens
import qualified Enecuum.Language                            as L

import qualified Enecuum.Samples.Assets.Blockchain.Generation        as A
import qualified Enecuum.Samples.Assets.Nodes.Address                as A
import           Enecuum.Samples.Assets.Nodes.Messages
import           Enecuum.Samples.Assets.Nodes.Methods                (handleStopNode, rpcPingPong)
import           Enecuum.Samples.Assets.Nodes.TstNodes.GenPoA.Config
import qualified Enecuum.Samples.Blockchain.Domain          as D

data TstGenPoANodeData = TstGenPoANodeData
    { _currentLastKeyBlock :: D.StateVar D.KBlock
    , _status              :: D.StateVar D.NodeStatus
    , _transactionPending  :: D.StateVar [D.Transaction]
    }

makeFieldsNoPrefix ''TstGenPoANodeData

instance Node TstGenPoANode where
    data NodeScenario TstGenPoANode = Good | Bad
        deriving (Show, Generic)
    getNodeScript = poaNode
    getNodeTag _ = TstGenPoANode

instance ToJSON   (NodeScenario TstGenPoANode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstGenPoANode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr D.showTransaction "" $ mBlock ^. Lens.transactions

sendMicroblock :: D.Address -> TstGenPoANodeData -> D.KBlock -> NodeScenario TstGenPoANode -> L.NodeL ()
sendMicroblock gnUdpAddress poaData block role = do
    currentBlock <- L.readVarIO (poaData ^. currentLastKeyBlock)
    when (block /= currentBlock) $ do
        L.logInfo $ "Empty KBlock found (" +|| toHash block ||+ ")."

        pendingTransactions <- L.atomically $ do
            tx <- take A.transactionsInMicroblock <$> L.readVar (poaData ^. transactionPending)
            L.modifyVar (poaData ^. transactionPending) (drop $ length tx)
            pure tx

        let pendingTransactionsCount = length pendingTransactions
        let transactionsCount = A.transactionsInMicroblock - pendingTransactionsCount

        when (pendingTransactionsCount > 0) $ L.logInfo $ "\nGet " +|| pendingTransactionsCount ||+ " transaction(s) from pending "
        when (transactionsCount        > 0) $ L.logInfo $ "Generate "  +|| transactionsCount ||+ " random transaction(s)."

        txGenerated <- replicateM transactionsCount $ A.genTransaction A.Hardcoded

        let tx = pendingTransactions ++ txGenerated

        L.writeVarIO (poaData ^. currentLastKeyBlock) block

        mBlock <- case role of
            Good -> A.genMicroblock block tx
            Bad  -> A.generateBogusSignedMicroblock block tx
        L.logInfo
            $ "MBlock generated (" +|| toHash mBlock ||+ ". Transactions:" +| showTransactions mBlock |+ ""
        void $ L.withConnection D.Udp gnUdpAddress $
            \conn -> L.send conn mBlock

poaNode :: NodeScenario TstGenPoANode -> NodeConfig TstGenPoANode -> L.NodeDefinitionL ()
poaNode role cfg = do
    L.setNodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    poaData <- L.atomically
        $ TstGenPoANodeData
            <$> L.newVar D.genesisKBlock
            <*> L.newVar D.NodeActing
            <*> L.newVar []

    L.std $ L.stdHandler $ L.stopNodeHandler poaData

    void $ L.serving D.Rpc (_controlRpcPort cfg) $ do
        L.method   rpcPingPong
        L.method $ handleStopNode poaData

    let gnRpcAddress = _genPoaGraphNodeRPCAddress cfg
    let gnUdpAddress = _genPoaGraphNodeUDPAddress cfg
    L.process $ forever $ do
        L.delay $ 100 * 1000
        whenRightM (L.makeRpcRequest gnRpcAddress GetTransactionPending) $ \tx -> do
            forM_ tx (\t -> L.logInfo $ "\nAdd transaction to pending "  +| D.showTransaction t "" |+ "")
            L.atomically $ L.modifyVar (poaData ^. transactionPending) ( ++ tx )
        whenRightM (L.makeRpcRequest gnRpcAddress GetLastKBlock) $ \block -> sendMicroblock gnUdpAddress poaData block role

    L.awaitNodeFinished poaData
