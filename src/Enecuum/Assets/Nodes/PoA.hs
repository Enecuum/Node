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
import           Enecuum.Assets.Nodes.Routing.Messages
import           Enecuum.Assets.Nodes.Routing.Runtime

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
    { _dummyOption :: Int
    , _poaRpcPort  :: D.PortNumber
    , _poaUdpPort  :: D.PortNumber
    , _poaTcpPort  :: D.PortNumber
    , _poaBnAddress :: D.Address
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

defaultPoANodeConfig = PoANodeConfig 42 A.poaNodeRpcPort A.poaNodeUdpPort A.poaNodeTcpPort A.bnAddress

showTransactions :: D.Microblock -> Text
showTransactions mBlock = foldr D.showTransaction "" $ mBlock ^. Lens.transactions

sendMicroblock :: RoutingRuntime -> PoANodeData -> NodeScenario PoANode -> D.KBlock -> L.NodeL ()
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




poaNode :: NodeScenario PoANode -> NodeConfig PoANode -> L.NodeDefinitionL ()
poaNode role cfg = do
    L.nodeTag "PoA node"
    L.logInfo "Starting of PoA node"
    let myNodePorts = NodePorts (_poaUdpPort cfg) (_poaTcpPort cfg) (_poaRpcPort cfg)
    let D.Address bnHost bnPort = _poaBnAddress cfg
    let bnPorts     = makeNodePorts1000 bnPort
    let bnId        = D.toHashGeneric bnPorts

    -- TODO: read from config
    let myHash      = D.toHashGeneric myNodePorts

    routingData <- L.scenario $ makeRoutingRuntimeData myNodePorts myHash (NodeAddress bnHost bnPorts bnId)
    poaData     <- L.scenario $ L.atomically (PoANodeData <$> L.newVar D.genesisKBlock <*> L.newVar NodeActing <*> L.newVar [])

    L.std $ L.stdHandler $ L.stopNodeHandler poaData

    L.serving D.Rpc (_poaRpcPort cfg) $ do
        rpcRoutingHandlers routingData
        L.method   rpcPingPong
        L.method $ handleStopNode poaData

    L.serving D.Udp  (_poaUdpPort cfg) $ do
        udpRoutingHandlers routingData
        L.handler $ udpBroadcastRecivedMessage routingData $
            sendMicroblock routingData poaData role

    routingWorker routingData
    L.awaitNodeFinished poaData
