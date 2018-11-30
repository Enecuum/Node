{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.RoutingNodes.GenPoW.Node where

import qualified Data.Aeson                                      as J
import           Data.HGraph.StringHashable                      (StringHash (..), toHash)
import qualified Enecuum.Assets.Blockchain.Generation            as A
import qualified Enecuum.Assets.Nodes.Address                    as A
import qualified Enecuum.Assets.Nodes.CLens                      as CLens
import qualified Enecuum.Assets.Nodes.Messages                   as Msgs
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.Routing
import           Enecuum.Assets.Nodes.RoutingNodes.GenPoW.Config
import           Enecuum.Config
import qualified Enecuum.Domain                                  as D
import           Enecuum.Framework.Language.Extra                (HasStatus)
import qualified Enecuum.Framework.Lens                          as Lens
import qualified Enecuum.Language                                as L
import           Enecuum.Prelude

type IterationsCount = Int
type EnableDelays = Bool

data GenPoWNodeData = GenPoWNodeData
    { _prevHash            :: D.StateVar StringHash
    , _prevNumber          :: D.StateVar D.BlockNumber
    , _requiredBlockNumber :: D.StateVar D.BlockNumber
    , _blocksDelay         :: D.StateVar Int        -- ^ delay between blocks, microseconds
    , _status              :: D.StateVar D.NodeStatus
    }

makeFieldsNoPrefix ''GenPoWNodeData

instance Node GenPoWNode where
    data NodeScenario GenPoWNode = PoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'
    getNodeTag _ = GenPoWNode

instance ToJSON   (NodeScenario GenPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario GenPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

kBlockProcess :: RoutingRuntime -> GenPoWNodeData -> L.NodeL ()
kBlockProcess routingData nodeData = do
    prevKBlockHash      <- L.readVarIO $ nodeData ^. prevHash
    prevKBlockNumber    <- L.readVarIO $ nodeData ^. prevNumber

    (lastHash, kBlocks) <- A.generateKBlocks prevKBlockHash prevKBlockNumber
    L.logInfo $ "Last hash: " +|| lastHash ||+ ", blocks count: " +|| length kBlocks ||+ "."

    L.writeVarIO (nodeData ^. prevHash) lastHash
    L.writeVarIO (nodeData ^. prevNumber) $ prevKBlockNumber + fromIntegral (length kBlocks)

    gap <- L.readVarIO $ nodeData ^. blocksDelay
    for_ kBlocks $ \ kBlock -> do
        L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
        sendUdpBroadcast routingData kBlock
        when (gap > 0) $ L.delay gap

foreverChainGenerationHandle :: GenPoWNodeData -> Msgs.ForeverChainGeneration -> L.NodeL Msgs.SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.writeVarIO (powNodeData ^. requiredBlockNumber) (10 ^ (6 :: Int))
    pure Msgs.SuccessMsg

nBlockPacketGenerationHandle :: GenPoWNodeData -> Msgs.NBlockPacketGeneration -> L.NodeL Msgs.SuccessMsg
nBlockPacketGenerationHandle powNodeData (Msgs.NBlockPacketGeneration i gap) = do
    L.atomically $ do
        L.modifyVar (powNodeData ^. requiredBlockNumber) (+ i)
        L.writeVar  (powNodeData ^. blocksDelay) gap
    pure Msgs.SuccessMsg


powNode :: L.NodeDefinitionL ()
powNode = powNode' routingGenPoWNodeConfig

powNode' :: NodeConfig GenPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "GenPoW node"

    let myNodePorts = _powNodePorts cfg
    let myHash      = _powNodeId cfg
    routingData <- runRouting myNodePorts myHash (_powNodebnAddress cfg)

    nodeData    <- L.initialization $ powNodeInitialization cfg D.genesisHash
    rpcServerOk <- L.serving D.Rpc (myNodePorts ^. Lens.nodeRpcPort) $ do
        rpcRoutingHandlers routingData
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

        -- client
        L.method  $ foreverChainGenerationHandle nodeData
        L.method  $ nBlockPacketGenerationHandle nodeData

    udpServerOk <- L.serving D.Udp (myNodePorts ^. Lens.nodeUdpPort) $
        udpRoutingHandlers routingData
    if all isJust [rpcServerOk, udpServerOk] then do
        L.std $ L.stdHandler $ L.stopNodeHandler nodeData
        L.process $ forever $ do
            L.atomically $ do
                i <- L.readVar $ nodeData ^. requiredBlockNumber
                when (i == 0) L.retry
                L.writeVar (nodeData ^. requiredBlockNumber) (i - 1)
            kBlockProcess routingData nodeData
        L.logInfo "GenPoW is ready to work."
        L.awaitNodeFinished nodeData
    else do
        unless (isJust rpcServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeRpcPort) "rpc"
        unless (isJust udpServerOk) $
            L.logError $ portError (myNodePorts ^. Lens.nodeUdpPort) "udp"


powNodeInitialization ::  NodeConfig GenPoWNode -> StringHash -> L.NodeL GenPoWNodeData
powNodeInitialization cfg genesisHash = do
    h <- L.newVarIO genesisHash
    n <- L.newVarIO 1
    b <- L.newVarIO 0
    g <- L.newVarIO $ _defaultBlocksDelay cfg
    f <- L.newVarIO D.NodeActing
    pure GenPoWNodeData
        { _prevHash = h
        , _prevNumber= n
        , _requiredBlockNumber = b
        , _blocksDelay = g
        , _status = f
        }
