{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW.PoW where

import           Enecuum.Prelude
import qualified Data.Aeson as A

import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D
import           Enecuum.Config
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address
import           Data.HGraph.StringHashable (StringHash (..), toHash)
import qualified Enecuum.Assets.Nodes.Messages as Msgs
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.PoW.Config
import qualified Enecuum.Assets.Nodes.CLens as CLens

type IterationsCount = Int
type EnableDelays = Bool

data PoWNodeData = PoWNodeData
    { _prevHash            :: D.StateVar StringHash
    , _prevNumber          :: D.StateVar D.BlockNumber
    , _requiredBlockNumber :: D.StateVar D.BlockNumber
    , _blocksDelay         :: D.StateVar Int        -- ^ delay between blocks, microseconds
    , _status              :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''PoWNodeData

instance Node PoWNode where
    data NodeScenario PoWNode = PoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'

instance ToJSON   (NodeScenario PoWNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PoWNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

kBlockProcess :: PoWNodeData -> L.NodeL ()
kBlockProcess nodeData = do
    prevKBlockHash      <- L.readVarIO $ nodeData ^. prevHash
    prevKBlockNumber    <- L.readVarIO $ nodeData ^. prevNumber

    (lastHash, kBlocks) <- A.generateKBlocks prevKBlockHash prevKBlockNumber
    L.logInfo $ "Last hash: " +|| lastHash ||+ "."

    L.writeVarIO (nodeData ^. prevHash) lastHash
    L.writeVarIO (nodeData ^. prevNumber) $ prevKBlockNumber + fromIntegral (length kBlocks)

    gap <- L.readVarIO $ nodeData ^. blocksDelay
    L.withConnection D.Tcp graphNodeTransmitterTcpAddress $ \conn -> for_ kBlocks $ \ kBlock -> do
            L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
            void $ L.send conn kBlock
            when (gap > 0) $ L.delay gap

foreverChainGenerationHandle :: PoWNodeData -> Msgs.ForeverChainGeneration -> L.NodeL Msgs.SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.writeVarIO (powNodeData ^. requiredBlockNumber) (10 ^ (6 :: Int))
    pure Msgs.SuccessMsg

nBlockPacketGenerationHandle :: PoWNodeData -> Msgs.NBlockPacketGeneration -> L.NodeL Msgs.SuccessMsg
nBlockPacketGenerationHandle powNodeData (Msgs.NBlockPacketGeneration i gap) = do
    L.atomically $ do
        L.modifyVar (powNodeData ^. requiredBlockNumber) (+ i)
        L.writeVar  (powNodeData ^. blocksDelay) gap
    pure Msgs.SuccessMsg

defaultBlocksDelay :: BlocksDelay
defaultBlocksDelay = 1000 * 1000

powNode :: L.NodeDefinitionL ()
powNode = powNode' $ PoWNodeConfig defaultBlocksDelay

powNode' :: NodeConfig PoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "PoW node"

    nodeData <- L.initialization $ powNodeInitialization cfg D.genesisHash
    L.serving D.Rpc powNodeRpcPort $ do
        L.method  $ foreverChainGenerationHandle nodeData
        L.method  $ nBlockPacketGenerationHandle nodeData
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.process $ forever $ do
        L.atomically $ do
            i <- L.readVar $ nodeData ^. requiredBlockNumber
            when (i == 0) L.retry
            L.writeVar (nodeData ^. requiredBlockNumber) (i - 1)
        kBlockProcess nodeData

    L.awaitNodeFinished nodeData

powNodeInitialization ::  NodeConfig PoWNode -> StringHash -> L.NodeL PoWNodeData
powNodeInitialization cfg genesisHash = do
    h <- L.newVarIO genesisHash
    n <- L.newVarIO 1
    b <- L.newVarIO 0
    g <- L.newVarIO $ cfg ^. CLens.defaultBlocksDelay
    f <- L.newVarIO NodeActing
    pure $ PoWNodeData
        { _prevHash = h
        , _prevNumber= n
        , _requiredBlockNumber = b
        , _blocksDelay = g
        , _status = f
        }
