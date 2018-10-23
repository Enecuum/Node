{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Enecuum.Assets.Nodes.GraphNodeReceiver (graphNodeReceiver) where

import           Data.HGraph.StringHashable
import           Data.Map                         (Map, fromList, insert, lookup, empty)
import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))
import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Messages
import qualified Enecuum.Blockchain.Domain.Graph  as TG
import qualified Enecuum.Blockchain.Lens          as Lens
import qualified Enecuum.Domain                   as D
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.Methodes

import qualified Enecuum.Framework.LogState as Log

data GraphNodeData = GraphNodeData
    { _blockchain    :: D.BlockchainData
    , _logVar        :: D.StateVar [Text]
    , _status        :: D.StateVar NodeStatus
    }
makeFieldsNoPrefix ''GraphNodeData

getLastKBlock :: GraphNodeData ->  GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    kBlock <- L.atomically $ L.getTopKeyBlock logV bData
    pure kBlock

getBalance :: GraphNodeData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| D.showPublicKey wallet ||+ "."
    let bData = nodeData ^. blockchain
    curLedger <- L.atomically $ L.readVar $ bData ^. Lens.ledger
    let maybeBalance = lookup wallet curLedger
    case maybeBalance of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left $ "Wallet " +|| D.showPublicKey wallet ||+ " does not exist in graph."

graphSynchro :: GraphNodeData -> D.Address -> L.NodeL ()
graphSynchro nodeData address = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain

    GetChainLengthResponse otherLength <- L.makeRpcRequestUnsafe address GetChainLengthRequest

    curChainLength <- L.atomically $ do
        topKBlock <- L.getTopKeyBlock logV bData
        pure $ topKBlock ^. Lens.number

    when (curChainLength < otherLength) $ do
        topNodeHash <- L.atomically $ L.readVar $ bData ^. Lens.curNode
        GetMBlocksForKBlockResponse mBlocks <- L.makeRpcRequestUnsafe address (GetMBlocksForKBlockRequest topNodeHash)
        L.logInfo $ "Mblocks received for kBlock " +|| topNodeHash ||+ " : " +|| show mBlocks
        L.atomically $ forM_ mBlocks (L.addMBlock logV bData)

        GetChainFromToResponse chainTail <- L.makeRpcRequestUnsafe address (GetChainFromToRequest (curChainLength + 1) otherLength)
        L.logInfo $ "Chain tail received from " +|| (curChainLength + 1) ||+ " to " +|| otherLength ||+ " : " +|| chainTail ||+ "."
        L.atomically $ forM_ chainTail (L.addKBlock logV bData)
        for_ (init chainTail) $ \kBlock -> do
            let hash = toHash kBlock
            GetMBlocksForKBlockResponse mBlocks <- L.makeRpcRequestUnsafe address (GetMBlocksForKBlockRequest hash)
            L.logInfo $ "Mblocks received for kBlock " +|| hash ||+ " : " +|| show mBlocks
            L.atomically $ forM_ mBlocks (L.addMBlock logV bData)
    Log.writeLog logV

-- | Initialization of graph node
graphNodeInitialization :: L.NodeDefinitionL GraphNodeData
graphNodeInitialization = L.scenario $ do
    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock
    L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."
    L.atomically
        $  GraphNodeData <$> (D.BlockchainData g
        <$> L.newVar []
        <*> L.newVar []
        <*> L.newVar D.genesisHash
        <*> L.newVar Data.Map.empty)
        <*> L.newVar []
        <*> L.newVar NodeActing

-- | Start of graph node
graphNodeReceiver :: L.NodeDefinitionL ()
graphNodeReceiver = do
    L.nodeTag "graphNodeReceiver"
    nodeData <- graphNodeInitialization

    L.process $ forever $ graphSynchro nodeData graphNodeTransmitterRpcAddress
    L.serving D.Rpc graphNodeReceiverRpcPort $ do
        L.methodE $ getBalance nodeData
        L.method  $ getLastKBlock nodeData
        L.method  $ rpcPingPong
        L.method  $ methodeStopNode nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.awaitNodeFinished nodeData
