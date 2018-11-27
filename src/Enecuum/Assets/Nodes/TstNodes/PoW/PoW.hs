{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.TstNodes.PoW.PoW where

import           Control.Lens                             (to)
import qualified Data.Aeson                               as J
import           Data.HGraph.StringHashable               (StringHash (..), toHash)
import qualified Enecuum.Assets.Blockchain.Generation     as A
import qualified Enecuum.Assets.Nodes.Messages            as Msgs
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.TstNodes.PoW.Config as Tst
import           Enecuum.Config
import qualified Enecuum.Domain                           as D
import           Enecuum.Framework.Language.Extra         (HasStatus)
import qualified Enecuum.Language                         as L
import           Enecuum.Prelude

type IterationsCount = Int
type EnableDelays = Bool

data TstPoWNodeData = TstPoWNodeData
    { _prevHash            :: D.StateVar StringHash
    , _prevNumber          :: D.StateVar D.BlockNumber
    , _requiredBlockNumber :: D.StateVar D.BlockNumber
    , _blocksDelay         :: D.StateVar Int        -- ^ delay between blocks, microseconds
    , _status              :: D.StateVar D.NodeStatus
    }

makeFieldsNoPrefix ''TstPoWNodeData

instance Node TstPoWNode where
    data NodeScenario TstPoWNode = PoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'
    getNodeTag _ = TstPoWNode

instance ToJSON   (NodeScenario TstPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

kBlockProcess :: D.Address -> TstPoWNodeData -> L.NodeL ()
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

foreverChainGenerationHandle :: TstPoWNodeData -> Msgs.ForeverChainGeneration -> L.NodeL Msgs.SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.writeVarIO (powNodeData ^. requiredBlockNumber) (10 ^ (6 :: Int))
    pure Msgs.SuccessMsg

nBlockPacketGenerationHandle :: TstPoWNodeData -> Msgs.NBlockPacketGeneration -> L.NodeL Msgs.SuccessMsg
nBlockPacketGenerationHandle powNodeData (Msgs.NBlockPacketGeneration i gap) = do
    L.atomically $ do
        L.modifyVar (powNodeData ^. requiredBlockNumber) (+ i)
        L.writeVar  (powNodeData ^. blocksDelay) gap
    pure Msgs.SuccessMsg


powNode :: L.NodeDefinitionL ()
powNode = powNode' defaultPoWNodeConfig

powNode' :: NodeConfig TstPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "Tst Gen PoW node"

    nodeData <- L.initialization $ powNodeInitialization cfg D.genesisHash
    L.serving D.Rpc (_powNodeRpcPort cfg) $ do
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
        kBlockProcess (_graphNodeUDPAddress cfg ) nodeData

    L.awaitNodeFinished nodeData

powNodeInitialization ::  NodeConfig TstPoWNode -> StringHash -> L.NodeL TstPoWNodeData
powNodeInitialization cfg genesisHash = do
    h <- L.newVarIO genesisHash
    n <- L.newVarIO 1
    b <- L.newVarIO 0
    g <- L.newVarIO $ cfg ^. to Tst._defaultBlocksDelay
    f <- L.newVarIO D.NodeActing
    pure TstPoWNodeData
        { _prevHash = h
        , _prevNumber= n
        , _requiredBlockNumber = b
        , _blocksDelay = g
        , _status = f
        }
