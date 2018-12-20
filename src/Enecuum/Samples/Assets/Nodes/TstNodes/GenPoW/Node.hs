{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Samples.Assets.Nodes.TstNodes.GenPoW.Node where

import           Control.Lens                                (to)
import qualified Data.Aeson                                  as J
import           Data.HGraph.StringHashable                  (StringHash (..), toHash)
import qualified Enecuum.Samples.Assets.Blockchain.Generation        as A
import qualified Enecuum.Samples.Assets.Nodes.Messages               as Msgs
import           Enecuum.Samples.Assets.Nodes.Methods
import           Enecuum.Samples.Assets.Nodes.TstNodes.GenPoW.Config as Tst
import           Enecuum.Config
import qualified Enecuum.Domain                              as D
import           Enecuum.Framework.Language.Extra            (HasStatus)
import qualified Enecuum.Language                            as L
import           Enecuum.Prelude
import qualified Enecuum.Samples.Blockchain.Domain          as D

type IterationsCount = Int
type EnableDelays = Bool

data TstGenPoWNodeData = TstGenPoWNodeData
    { _prevHash            :: D.StateVar StringHash
    , _prevNumber          :: D.StateVar D.BlockNumber
    , _requiredBlockNumber :: D.StateVar D.BlockNumber
    , _blocksDelay         :: D.StateVar Int        -- ^ delay between blocks, microseconds
    , _status              :: D.StateVar D.NodeStatus
    }

makeFieldsNoPrefix ''TstGenPoWNodeData

instance Node TstGenPoWNode where
    data NodeScenario TstGenPoWNode = PoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'
    getNodeTag _ = TstGenPoWNode

instance ToJSON   (NodeScenario TstGenPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstGenPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

kBlockProcess :: D.Address -> TstGenPoWNodeData -> L.NodeL ()
kBlockProcess graphNodeUdpAddress nodeData = do
    prevKBlockHash      <- L.readVarIO $ nodeData ^. prevHash
    prevKBlockNumber    <- L.readVarIO $ nodeData ^. prevNumber

    (lastHash, kBlocks) <- A.generateKBlocks prevKBlockHash prevKBlockNumber
    L.logInfo $ "Last hash: " +|| lastHash ||+ "."

    L.writeVarIO (nodeData ^. prevHash) lastHash
    L.writeVarIO (nodeData ^. prevNumber) $ prevKBlockNumber + fromIntegral (length kBlocks)

    gap <- L.readVarIO $ nodeData ^. blocksDelay
    for_ kBlocks $ \ kBlock -> L.withConnection D.Udp graphNodeUdpAddress $ \conn -> do
            L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
            void $ L.send conn kBlock
            when (gap > 0) $ L.delay gap

foreverChainGenerationHandle :: TstGenPoWNodeData -> Msgs.ForeverChainGeneration -> L.NodeL D.SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.writeVarIO (powNodeData ^. requiredBlockNumber) (10 ^ (6 :: Int))
    pure D.SuccessMsg

nBlockPacketGenerationHandle :: TstGenPoWNodeData -> Msgs.NBlockPacketGeneration -> L.NodeL D.SuccessMsg
nBlockPacketGenerationHandle powNodeData (Msgs.NBlockPacketGeneration i gap) = do
    L.atomically $ do
        L.modifyVar (powNodeData ^. requiredBlockNumber) (+ i)
        L.writeVar  (powNodeData ^. blocksDelay) gap
    pure D.SuccessMsg


powNode :: L.NodeDefinitionL ()
powNode = powNode' tstGenPoWNodeConfig

powNode' :: NodeConfig TstGenPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.setNodeTag "Tst Gen PoW node"

    nodeData <- L.initialization $ powNodeInitialization cfg D.genesisHash
    L.serving D.Rpc (_controlRpcPort cfg) $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

        -- client
        L.method  $ foreverChainGenerationHandle nodeData
        L.method  $ nBlockPacketGenerationHandle nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.process $ forever $ do
        L.atomically $ do
            i <- L.readVar $ nodeData ^. requiredBlockNumber
            when (i == 0) L.retry
            L.writeVar (nodeData ^. requiredBlockNumber) (i - 1)
        kBlockProcess (_genPowGraphNodeUDPAddress cfg) nodeData

    L.awaitNodeFinished nodeData

powNodeInitialization ::  NodeConfig TstGenPoWNode -> StringHash -> L.NodeL TstGenPoWNodeData
powNodeInitialization cfg genesisHash = do
    h <- L.newVarIO genesisHash
    n <- L.newVarIO 1
    b <- L.newVarIO 0
    g <- L.newVarIO $ cfg ^. to Tst._defaultBlocksDelay
    f <- L.newVarIO D.NodeActing
    pure TstGenPoWNodeData
        { _prevHash = h
        , _prevNumber= n
        , _requiredBlockNumber = b
        , _blocksDelay = g
        , _status = f
        }
