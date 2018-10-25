{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.GraphNode.Logic where

import           Control.Lens                     (makeFieldsNoPrefix)
import           Data.HGraph.StringHashable
import qualified Data.Map                         as Map
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))
import qualified Enecuum.Framework.LogState       as Log
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import qualified Enecuum.Blockchain.Lens          as Lens

data GraphNodeData = GraphNodeData
    { _blockchain :: D.BlockchainData
    , _logVar     :: D.StateVar [Text]
    , _status     :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''GraphNodeData

transactionsToTransfer = 20

-- | Accept transaction
acceptTransaction :: GraphNodeData -> CreateTransaction -> L.NodeL (Either Text SuccessMsg)
acceptTransaction nodeData (CreateTransaction tx) = do
    L.logInfo $ "Got transaction "  +| D.showTransaction tx "" |+ ""
    if L.verifyTransaction tx
        then do
            L.logInfo $ "\nTransaction is accepted"
            L.logInfo $ "\nAdd transaction to pending "
            let bData = nodeData ^. blockchain
            L.atomically $ do
                L.modifyVar (bData ^. Lens.transactionPending) ( Map.insert (toHash tx ) tx)
            pure $ Right SuccessMsg
        else do
            L.logInfo $ "Transaction signature is not genuine"
            L.logInfo $ "Transaction is not accepted"
            pure $ Left "Transaction signature is not genuine. Transaction is not accepted."

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
    let res@(valid, _, _) = L.verifyMicroblockWithTx mBlock
    when (not valid) $ printInvalidSignatures res
    when valid $ do
        L.logInfo $ "Microblock " +|| toHash mBlock ||+ " is accepted."
        let logV = nodeData ^. logVar
            bData = nodeData ^. blockchain
        void $ L.atomically $ do
            L.addMBlock logV bData mBlock
            let bData = nodeData ^. blockchain
            let tx = mBlock ^. Lens.transactions
            let fun :: D.Transaction -> D.TransactionPending -> D.TransactionPending
                fun t pending = Map.delete (toHash t) pending
            forM_ tx (\t -> L.modifyVar (bData ^. Lens.transactionPending) (fun t ))
        Log.writeLog logV
    where
        printInvalidSignatures :: (Bool, Bool, [Bool]) -> L.NodeL ()
        printInvalidSignatures (valid, mBlockValid, txsValid) = do
            when (not valid)           $ L.logInfo $ "Microblock is rejected: " +|| toHash mBlock ||+ "."
            when (not mBlockValid)     $ L.logInfo $ "Microblock has " +|| toHash mBlock ||+ " invalid signature."
            when (elem False txsValid) $ L.logInfo $ "Microblock " +|| toHash mBlock ||+ " transactions have invalid signature."

getKBlockPending :: GraphNodeData -> GetKBlockPending -> L.NodeL [D.KBlock]
getKBlockPending nodeData _ = do
    let bData = nodeData ^. blockchain
    kBlocks <- L.readVarIO $ bData ^. Lens.kBlockPending
        -- kblocks <- L.readVar $ bData ^. Lens.kBlockPending
        -- L.modifyVar (bData ^. Lens.kBlockPending) (\_ -> [])
        -- pure kblocks
    pure kBlocks

getTransactionPending :: GraphNodeData -> GetTransactionPending -> L.NodeL [D.Transaction]
getTransactionPending nodeData _ = do
    let bData = nodeData ^. blockchain
    L.atomically $ do
        trans <- L.readVar $ bData ^. Lens.transactionPending
        pure $ map snd $ take transactionsToTransfer $ Map.toList trans

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
    curLedger <- L.readVarIO $ bData ^. Lens.ledger
    let maybeBalance = Map.lookup wallet curLedger
    case maybeBalance of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left $ "Wallet " +|| D.showPublicKey wallet ||+ " does not exist in graph."

getChainLength :: GraphNodeData -> GetChainLengthRequest -> L.NodeL GetChainLengthResponse
getChainLength nodeData GetChainLengthRequest = do
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

getMBlockForKBlocks :: GraphNodeData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
getMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Answering microblocks for kBlock " +|| show hash
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    mBlockList <- L.atomically $ L.getMBlocksForKBlock logV bData hash
    Log.writeLog logV
    case mBlockList of
        Nothing        -> pure $ Left "KBlock doesn't exist"
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
        <*> L.newVar Map.empty
        <*> L.newVar D.genesisHash
        <*> L.newVar Map.empty)
        <*> L.newVar []
        <*> L.newVar NodeActing
