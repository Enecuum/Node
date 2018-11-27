{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.TstNodes.PoA where

import qualified Data.Aeson                           as A
import           Enecuum.Prelude

import           Data.HGraph.StringHashable           (toHash)

import qualified Enecuum.Blockchain.Lens              as Lens
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import           Enecuum.Framework.Language.Extra     (HasStatus)
import qualified Enecuum.Framework.Lens               as Lens
import qualified Enecuum.Language                     as L

import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Assets.Nodes.Address         as A
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Methods         (handleStopNode, rpcPingPong)

data TstPoaNodeData = TstPoaNodeData
    { _currentLastKeyBlock :: D.StateVar D.KBlock
    , _status              :: D.StateVar D.NodeStatus
    , _transactionPending  :: D.StateVar [D.Transaction]
    }

makeFieldsNoPrefix ''TstPoaNodeData

data TstPoaNode = TstPoaNode
    deriving (Show, Generic)

data instance NodeConfig TstPoaNode = TstPoANodeConfig
    { _poaRPCPort  :: D.PortNumber
    }
    deriving (Show, Generic)

instance Node TstPoaNode where
    data NodeScenario TstPoaNode = Good | Bad
        deriving (Show, Generic)
    getNodeScript = poaNode
    getNodeTag _ = TstPoaNode

instance ToJSON   TstPoaNode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON TstPoaNode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig TstPoaNode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig TstPoaNode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario TstPoaNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstPoaNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

defaultPoANodeConfig :: NodeConfig TstPoaNode
defaultPoANodeConfig = TstPoANodeConfig (A.defaultPoANodePorts ^. Lens.nodeRpcPort)

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr D.showTransaction "" $ mBlock ^. Lens.transactions

sendMicroblock :: TstPoaNodeData -> D.KBlock -> NodeScenario TstPoaNode -> L.NodeL ()
sendMicroblock poaData block role = do
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
        let gnUdpAddress = A.getUdpAddress A.defaultGnNodeAddress
        void $ L.withConnection D.Udp gnUdpAddress $
            \conn -> L.send conn mBlock

poaNode :: NodeScenario TstPoaNode -> NodeConfig TstPoaNode -> L.NodeDefinitionL ()
poaNode role cfg = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    poaData <- L.atomically
        $ TstPoaNodeData
            <$> L.newVar D.genesisKBlock
            <*> L.newVar D.NodeActing
            <*> L.newVar []

    L.std $ L.stdHandler $ L.stopNodeHandler poaData

    void $ L.serving D.Rpc (_poaRPCPort cfg) $ do
        L.method   rpcPingPong
        L.method $ handleStopNode poaData

    let gnRpcAddress = A.getRpcAddress A.defaultGnNodeAddress
    L.process $ forever $ do
        L.delay $ 100 * 1000
        whenRightM (L.makeRpcRequest gnRpcAddress GetTransactionPending) $ \tx -> do
            forM_ tx (\t -> L.logInfo $ "\nAdd transaction to pending "  +| D.showTransaction t "" |+ "")
            L.atomically $ L.modifyVar (poaData ^. transactionPending) ( ++ tx )
        whenRightM (L.makeRpcRequest gnRpcAddress GetLastKBlock) $ \block -> sendMicroblock poaData block role

    L.awaitNodeFinished poaData
