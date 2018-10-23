{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}

module Enecuum.Assets.Nodes.GraphNodeTransmitter (graphNodeTransmitter) where

import           Enecuum.Prelude
import Data.Map (fromList, lookup, insert, Map(..), elems, keys, empty)
import           Control.Lens                  (makeFieldsNoPrefix)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Data.HGraph.StringHashable

import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Address

import qualified Enecuum.Framework.LogState as Log
import           Enecuum.Assets.Nodes.Methodes


data GraphNodeData = GraphNodeData
    { _blockchain    :: D.BlockchainData
    , _logVar        :: D.StateVar [Text]
    , _status        :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''GraphNodeData

-- | Accept transaction
acceptTransaction :: GraphNodeData -> AcceptTransaction -> L.NodeL SuccessMsg
acceptTransaction nodeData (AcceptTransaction tx) = do
    L.logInfo $ "\nAccept transaction: " +| D.showTx tx |+ ""
    L.logInfo $ "\nAdd transaction to pending "    
    let bData = nodeData ^. blockchain
    L.atomically $ do
        pending <- L.readVar (bData ^. Lens.transactionPending)
        L.writeVar (bData ^. Lens.transactionPending) ( tx : pending )
    pure SuccessMsg

-- | Accept kBlock
acceptKBlock :: GraphNodeData -> D.KBlock -> D.Connection D.Tcp -> L.NodeL ()
acceptKBlock nodeData kBlock _ = do
    L.logInfo $ "\nAccepting KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    res <- L.atomically $ L.addKBlock logV bData kBlock
    Log.writeLog logV


-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> D.Connection D.Tcp -> L.NodeL ()
acceptMBlock nodeData mBlock _ = do
    isSignGenuine <- D.verifyMicroblockWithTxEff mBlock
    if not isSignGenuine
        then L.logInfo $ "Microblock is not accepted."
        else do
            L.logInfo "Microblock is accepted."
            let logV = nodeData ^. logVar
                bData = nodeData ^. blockchain
            void $ L.atomically (L.addMBlock logV bData mBlock)
            Log.writeLog logV

getKBlockPending :: GraphNodeData -> GetKBlockPending -> L.NodeL [D.KBlock]
getKBlockPending nodeData _ = do
    let bData = nodeData ^. blockchain 
    kBlocks <- L.atomically $ L.readVar $ bData ^. Lens.kBlockPending
        -- kblocks <- L.readVar $ bData ^. Lens.kBlockPending
        -- L.modifyVar (bData ^. Lens.kBlockPending) (\_ -> [])     
        -- pure kblocks
    pure kBlocks

getTransactionPending :: GraphNodeData -> GetTransactionPending -> L.NodeL [D.Transaction]
getTransactionPending nodeData _ = do
    let bData = nodeData ^. blockchain 
    tx <- L.atomically $ do
        trans <- L.readVar $ bData ^. Lens.transactionPending
        L.modifyVar (bData ^. Lens.transactionPending) (\_ -> [])     
        pure trans
    pure tx

getLastKBlock :: GraphNodeData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    -- L.logInfo "Top KBlock requested."
    kBlock <- L.atomically $ L.getTopKeyBlock logV bData
    -- L.logInfo $ "Top KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
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

acceptChainLength :: GraphNodeData -> GetChainLengthRequest -> L.NodeL GetChainLengthResponse
acceptChainLength nodeData GetChainLengthRequest = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
--    L.logInfo "Answering chain length"
    topKBlock <- L.atomically $ L.getTopKeyBlock logV bData
    Log.writeLog logV
    pure $ GetChainLengthResponse $ topKBlock ^. Lens.number


acceptChainFromTo :: GraphNodeData -> GetChainFromToRequest -> L.NodeL (Either Text GetChainFromToResponse)
acceptChainFromTo nodeData (GetChainFromToRequest from to) = do
    L.logInfo $ "Answering chain from " +|| show from ||+ " to " +|| show to
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    if from > to
        then pure $ Left "From is greater than to"
        else do
            kBlockList <- L.atomically $ do
                topKBlock <- L.getTopKeyBlock logV bData
                chain <- L.findBlocksByNumber logV bData from topKBlock
                pure $ drop (fromEnum $ (topKBlock ^. Lens.number) - to) chain
            Log.writeLog logV
            pure $ Right $ GetChainFromToResponse (reverse kBlockList)

acceptMBlockForKBlocks :: GraphNodeData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
acceptMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Answering microblocks for kBlock " +|| show hash
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    mBlockList <- L.atomically $ L.getMBlocksForKBlock logV bData hash
    Log.writeLog logV
    case mBlockList of
        Nothing -> pure $ Left "KBlock doesn't exist"
        Just blockList -> pure $ Right $ GetMBlocksForKBlockResponse blockList

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
graphNodeTransmitter :: L.NodeDefinitionL ()
graphNodeTransmitter = do
    L.nodeTag "graphNodeTransmitter"
    nodeData <- graphNodeInitialization

    L.serving D.Tcp graphNodeTransmitterTcpPort $ do
        L.handler $ acceptMBlock nodeData
        L.handler $ acceptKBlock nodeData
        L.handler $ methodePing

    L.serving D.Rpc graphNodeTransmitterRpcPort $ do
        L.method  $ getLastKBlock nodeData
        L.method  $ getKBlockPending nodeData
        L.method  $ getTransactionPending nodeData        
        L.methodE $ getBalance nodeData
        L.method  $ acceptChainLength nodeData
        L.methodE $ acceptChainFromTo nodeData
        L.methodE $ acceptMBlockForKBlocks nodeData
        L.method  $ rpcPingPong
        L.method  $ methodeStopNode nodeData
        L.method  $ acceptTransaction nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.awaitNodeFinished nodeData
