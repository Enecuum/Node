{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import Enecuum.Prelude
import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D
import qualified Enecuum.Assets.Blockchain.Generation as A
import           Enecuum.Assets.Nodes.Address (powNodeRpcPort, graphNodeTransmitterTcpAddress)
import           Data.HGraph.StringHashable (StringHash (..), toHash)
import           Enecuum.Assets.Nodes.Messages (
    SuccessMsg (..), ForeverChainGeneration(..), NBlockPacketGeneration(..))
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))
import           Enecuum.Assets.Nodes.Methods

type IterationsCount = Int
type EnableDelays = Bool

data PoWNodeData = PoWNodeData
    { _enableDelays        :: EnableDelays
    , _prevHash            :: D.StateVar StringHash
    , _prevNumber          :: D.StateVar Integer
    , _requiredBlockNumber :: D.StateVar Int
    , _status              :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''PoWNodeData

kBlockProcess :: PoWNodeData -> L.NodeL ()
kBlockProcess nodeData = do
    prevKBlockHash      <- L.readVarIO $ nodeData ^. prevHash
    prevKBlockNumber    <- L.readVarIO $ nodeData ^. prevNumber

    (lastHash, kBlocks) <- A.generateKBlocks prevKBlockHash prevKBlockNumber
    L.logInfo $ "Last hash: " +|| lastHash ||+ "."

    L.writeVarIO (nodeData ^. prevHash) lastHash
    L.writeVarIO (nodeData ^. prevNumber) $ prevKBlockNumber + fromIntegral (length kBlocks)
    for_ kBlocks $ \ kBlock -> L.withConnection D.Tcp graphNodeTransmitterTcpAddress $ \conn -> do
            L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
            void $ L.send conn kBlock
            when (nodeData ^. enableDelays) $ L.delay $ 1000 * 1000

foreverChainGenerationHandle :: PoWNodeData -> ForeverChainGeneration -> L.NodeL SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.writeVarIO (powNodeData ^. requiredBlockNumber) (10 ^ (6 :: Int))
    pure SuccessMsg

nBlockPacketGenerationHandle :: PoWNodeData -> NBlockPacketGeneration -> L.NodeL SuccessMsg
nBlockPacketGenerationHandle powNodeData (NBlockPacketGeneration i) = do
    L.atomically $ L.modifyVar (powNodeData ^. requiredBlockNumber) (+ i)
    pure SuccessMsg

powNode :: L.NodeDefinitionL ()
powNode = powNode' True

powNode' :: EnableDelays -> L.NodeDefinitionL ()
powNode' delaysEnabled = do
    L.nodeTag "PoW node"

    nodeData <- L.initialization $ powNodeInitialization delaysEnabled D.genesisHash
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

powNodeInitialization :: EnableDelays -> StringHash -> L.NodeL PoWNodeData
powNodeInitialization delaysEnabled genesisHash = do
    h <- L.newVarIO genesisHash
    n <- L.newVarIO 1
    b <- L.newVarIO 0
    f <- L.newVarIO NodeActing
    pure $ PoWNodeData delaysEnabled h n b f
