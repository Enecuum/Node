{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoA where

import           Enecuum.Prelude
import qualified Data.Aeson as A

import           Data.HGraph.StringHashable   (toHash)

import qualified Enecuum.Domain               as D
import           Enecuum.Config
import qualified Enecuum.Language             as L
import qualified Enecuum.Blockchain.Lens      as Lens
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))

import qualified Enecuum.Assets.Nodes.Address as A
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Methods (rpcPingPong, handleStopNode)
import qualified Enecuum.Assets.Blockchain.Generation as A

data PoANodeData = PoANodeData
    { _currentLastKeyBlock :: D.StateVar D.KBlock
    , _status              :: D.StateVar NodeStatus
    , _transactionPending  :: D.StateVar [D.Transaction]
    }

makeFieldsNoPrefix ''PoANodeData

data PoANode = PoANode
    deriving (Show, Generic)

data instance NodeConfig PoANode = PoANodeConfig
    { dummyOption :: Int
    }
    deriving (Show, Generic)

instance Node PoANode where
    data NodeScenario PoANode = Good | Bad
        deriving (Show, Generic)
    getNodeScript = poaNode

instance ToJSON   PoANode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON PoANode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig PoANode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PoANode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario PoANode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PoANode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr D.showTransaction "" $ mBlock ^. Lens.transactions

sendMicroblock :: PoANodeData -> D.KBlock -> NodeScenario PoANode -> L.NodeL ()
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
        void $ L.withConnection D.Tcp A.graphNodeTransmitterTcpAddress $
            \conn -> L.send conn mBlock

poaNode :: NodeScenario PoANode -> NodeConfig PoANode -> L.NodeDefinitionL ()
poaNode role _ = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    poaData <- L.scenario $ L.atomically (PoANodeData <$> L.newVar D.genesisKBlock <*> L.newVar NodeActing <*> L.newVar [])

    L.std $ L.stdHandler $ L.stopNodeHandler poaData

        L.method   rpcPingPong

    L.process $ forever $ do
        L.delay $ 100 * 1000
        whenRightM (L.makeRpcRequest A.graphNodeTransmitterRpcAddress GetTransactionPending) $ \tx -> do
            forM_ tx (\t -> L.logInfo $ "\nAdd transaction to pending "  +| D.showTransaction t "" |+ "")
            L.atomically $ L.modifyVar (poaData ^. transactionPending) ( ++ tx )
        whenRightM (L.makeRpcRequest A.graphNodeTransmitterRpcAddress GetLastKBlock) $ \block -> sendMicroblock poaData block role

    L.awaitNodeFinished poaData
