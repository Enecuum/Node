{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import Enecuum.Prelude
import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D

import           Enecuum.Assets.Nodes.Address (graphNodeTransmitterRpcAddress, powNodeRpcPort, graphNodeTransmitterTcpAddress)
import           Data.HGraph.StringHashable (StringHash (..), toHash)
import           Enecuum.Assets.Nodes.Messages (
    SuccessMsg (..), ForeverChainGeneration(..), NBlockPacketGeneration(..))
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))

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
    prevKBlockHash      <- L.atomically <$> L.readVar $ nodeData ^. prevHash
    prevKBlockNumber    <- L.atomically <$> L.readVar $ nodeData ^. prevNumber

    (lastHash, kBlocks) <- D.generateKBlocks prevKBlockHash prevKBlockNumber
    L.logInfo $ "Last hash: " +|| lastHash ||+ "."

    L.atomically $ L.writeVar (nodeData ^. prevHash) lastHash
    L.atomically $ L.writeVar (nodeData ^. prevNumber) $ prevKBlockNumber + (fromIntegral $ length kBlocks)
    conn <- L.open D.Tcp graphNodeTransmitterTcpAddress $ pure ()
    forM_ kBlocks $ \kBlock -> do
        L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
        L.send conn kBlock
        when (nodeData ^. enableDelays) $ L.delay $ 1000 * 1000
    L.close conn

foreverChainGenerationHandle :: PoWNodeData -> ForeverChainGeneration -> L.NodeL SuccessMsg
foreverChainGenerationHandle powNodeData _ = do
    L.atomically $ L.writeVar (powNodeData ^. requiredBlockNumber) (10 ^ (6 :: Int))
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
        L.method $ foreverChainGenerationHandle nodeData
        L.method $ nBlockPacketGenerationHandle nodeData

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
    h <- L.atomically $ L.newVar genesisHash
    n <- L.atomically $ L.newVar 1
    b <- L.atomically $ L.newVar 0
    f <- L.atomically $ L.newVar NodeActing
    pure $ PoWNodeData delaysEnabled h n b f
