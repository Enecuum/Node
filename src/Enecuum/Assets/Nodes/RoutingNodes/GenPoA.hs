{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.RoutingNodes.GenPoA where

import qualified Data.Aeson                           as A
import           Enecuum.Prelude

import           Data.HGraph.StringHashable           (toHash)

import qualified Enecuum.Blockchain.Lens              as Lens
import           Enecuum.Config
import qualified Enecuum.Domain                       as D
import           Enecuum.Framework.Language.Extra     (HasStatus)
import qualified Enecuum.Framework.Lens               as Lens
import qualified Enecuum.Language                     as L

import qualified Enecuum.Assets.Nodes.Address         as A
import           Enecuum.Assets.Nodes.Routing

import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Methods         (handleStopNode, portError, rpcPingPong)

data GenPoANodeData = GenPoANodeData
    { _currentLastKeyBlock :: D.StateVar D.KBlock
    , _status              :: D.StateVar D.NodeStatus
    , _transactionPending  :: D.StateVar [D.Transaction]
    }

makeFieldsNoPrefix ''GenPoANodeData

data GenPoANode = GenPoANode
    deriving (Show, Generic)

data instance NodeConfig GenPoANode = GenPoANodeConfig
    { _poaNodePorts :: D.NodePorts
    , _poaBnAddress :: D.NodeAddress
    , _poaNodeId    :: D.NodeId
    }
    deriving (Show, Generic)

instance Node GenPoANode where
    data NodeScenario GenPoANode = Good | Bad
        deriving (Show, Generic)
    getNodeScript = poaNode
    getNodeTag _ = GenPoANode

instance ToJSON   GenPoANode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON GenPoANode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig GenPoANode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig GenPoANode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario GenPoANode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario GenPoANode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

routingGenPoANodeConfig :: NodeConfig GenPoANode
routingGenPoANodeConfig = GenPoANodeConfig A.routingGenPoANodePorts A.routingBootNodeAddress (D.toHashGeneric A.routingGenPoANodePorts)

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr D.showTransaction "" $ mBlock ^. Lens.transactions

sendMicroblock :: RoutingRuntime -> GenPoANodeData -> NodeScenario GenPoANode -> D.KBlock -> L.NodeL ()
sendMicroblock routingData poaData role block = do
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

        void $ sendUdpBroadcast routingData mBlock


poaNode :: NodeScenario GenPoANode -> NodeConfig GenPoANode -> L.NodeDefinitionL ()
poaNode role cfg = do
    L.nodeTag "GenPoA node"
    L.logInfo "Starting of GenPoA node"
    let myNodePorts = _poaNodePorts cfg
    let myHash      = _poaNodeId cfg

    routingData <- runRouting myNodePorts myHash (_poaBnAddress cfg)
    poaData     <- L.atomically $
        GenPoANodeData
          <$> L.newVar D.genesisKBlock
          <*> L.newVar D.NodeActing
          <*> L.newVar []

    L.std $ L.stdHandler $ L.stopNodeHandler poaData

    rpcServerOk <- L.serving D.Rpc (myNodePorts ^. Lens.nodeRpcPort) $ do
        rpcRoutingHandlers routingData
        L.method   rpcPingPong
        L.method $ handleStopNode poaData

    udpServerOk <- L.serving D.Udp (myNodePorts ^. Lens.nodeUdpPort) $ do
        udpRoutingHandlers routingData
        L.handler $ udpBroadcastReceivedMessage routingData $
            sendMicroblock routingData poaData role
    if all isJust [rpcServerOk, udpServerOk] then L.awaitNodeFinished poaData
    else do
        unless (isJust rpcServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeRpcPort) "rpc"
        unless (isJust udpServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeUdpPort) "udp"
