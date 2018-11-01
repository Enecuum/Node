{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}

module Enecuum.Assets.Nodes.GraphNode.Receiver where

import           Data.HGraph.StringHashable
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.GraphNode.Logic
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Blockchain.Lens              as Lens
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Framework.LogState           as Log
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.GraphNode.Config

graphSynchro :: GraphNodeData -> D.Address -> L.NodeL ()
graphSynchro nodeData address = do
    let logV  = nodeData ^. logVar
    let bData = nodeData ^. blockchain

    GetChainLengthResponse otherLength <- L.makeRpcRequestUnsafe address GetChainLengthRequest

    curChainLength <- L.atomically $ do
        topKBlock <- L.getTopKeyBlock logV bData
        pure $ topKBlock ^. Lens.number

    when (curChainLength < otherLength) $ do

        topNodeHash <- L.readVarIO $ bData ^. Lens.curNode

        GetMBlocksForKBlockResponse mBlocks1 <- L.makeRpcRequestUnsafe address (GetMBlocksForKBlockRequest topNodeHash)
        L.logInfo $ "Mblocks received for kBlock " +|| topNodeHash ||+ " : " +|| mBlocks1 ||+ "."
        L.atomically $ forM_ mBlocks1 (L.addMBlock logV bData)

        GetChainFromToResponse chainTail <- L.makeRpcRequestUnsafe address (GetChainFromToRequest (curChainLength + 1) otherLength)
        L.logInfo $ "Chain tail received from " +|| (curChainLength + 1) ||+ " to " +|| otherLength ||+ " : " +|| chainTail ||+ "."
        L.atomically $ forM_ chainTail (L.addKBlock logV bData)

        for_ (init chainTail) $ \kBlock -> do

            L.atomically $ void $ L.addKBlock logV bData kBlock

            let hash = toHash kBlock
            GetMBlocksForKBlockResponse mBlocks2 <- L.makeRpcRequestUnsafe address (GetMBlocksForKBlockRequest hash)

            L.logInfo $ "Mblocks received for kBlock " +|| hash ||+ " : " +|| mBlocks2 ||+ "."
            L.atomically $ forM_ mBlocks2 (L.addMBlock logV bData)

        L.atomically $ void $ L.addKBlock logV bData (last chainTail)
    Log.writeLog logV


-- | Start of graph node
graphNodeReceiver :: NodeConfig GraphNode -> L.NodeDefinitionL ()
graphNodeReceiver _ = do
    L.nodeTag "graphNodeReceiver"
    nodeData <- graphNodeInitialization

    L.process $ forever $ graphSynchro nodeData graphNodeTransmitterRpcAddress
    L.serving D.Rpc graphNodeReceiverRpcPort $ do
        -- network
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

        -- client interaction
        L.methodE $ getBalance nodeData

        -- graph node interaction
        L.method  $ getChainLength nodeData
        L.methodE $ getMBlockForKBlocks nodeData

        -- PoA interaction
        L.method  $ getLastKBlock nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.awaitNodeFinished nodeData
