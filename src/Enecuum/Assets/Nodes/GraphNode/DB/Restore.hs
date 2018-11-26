{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.GraphNode.DB.Restore where

import           Enecuum.Prelude

import qualified Enecuum.Blockchain.DB                           as D
import qualified Enecuum.Blockchain.DB.Lens                      as Lens
import qualified Enecuum.Blockchain.Lens                         as Lens
import qualified Enecuum.Domain                                  as D
import qualified Enecuum.Language                                as L

import           Enecuum.Assets.Nodes.GraphNode.DB.Helpers
import qualified Enecuum.Assets.Nodes.GraphNode.GraphServiceData as G
import qualified Enecuum.Assets.Nodes.GraphNode.Logic            as G

loadKBlockMeta :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadKBlockMeta dbModel hash = do
    eKBlockMeta <- withKBlocksMetaDB dbModel $ L.getValue' hash
    whenRight eKBlockMeta $ \kBlockMeta -> L.logDebug
        $  "\n    (" +|| hash ||+ ") KBlock meta"
        <> "\n    " <> show kBlockMeta
    pure eKBlockMeta

loadKBlock :: D.DBModel -> D.DBValue D.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock dbModel (D.KBlockMetaValue i) = do
    ePrevHash     <- withKBlocksDB dbModel $ L.getValue' @D.KBlockPrevHashEntity i
    whenRight ePrevHash $ \prevHash -> L.logDebug
        $  "\n    [" +|| i ||+ "] KBlock hash entity"
        <> "\n    " <> show prevHash

    eKBlockEntity <- withKBlocksDB dbModel $ L.getValue' @D.KBlockEntity i
    whenRight eKBlockEntity $ \ kBlockEntity -> L.logDebug
        $  "\n    [" +|| i ||+ "]: KBlock entity"
        <> "\n    " <> show kBlockEntity

    pure $ D.KBlock
        <$> (eKBlockEntity ^. Lens.time'    )
        <*> (ePrevHash     ^. Lens.prevHash')
        <*> (eKBlockEntity ^. Lens.number'  )
        <*> (eKBlockEntity ^. Lens.nonce'   )
        <*> (eKBlockEntity ^. Lens.solver'  )

-- We don't know how many mblocks exists. We also don't know their indexes,
-- But we know the indexes are counting from 1 to N.
-- We will be loading them all untill the KeyNotFound is reached.
-- The same is for transactions.

loadTransactions'
    :: D.DBModel
    -> (D.KBlockIdx, D.MBlockIdx, D.TransactionIdx)
    -> L.NodeL [D.Transaction]
loadTransactions' dbModel fullIdx@(kBlockIdx, mBlockIdx, transactionIdx) = do
    eDBTx <- withTransactionsDB dbModel $ L.getValue' @D.TransactionEntity fullIdx
    whenRight eDBTx $ \dbTx -> L.logDebug
        $  "\n    [" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] [" +|| transactionIdx ||+ "] Transaction entity"
        <> "\n    " <> show dbTx

    case eDBTx of
        Right dbTx -> do
            let tx = D.fromDBTransaction dbTx
            txs <- loadTransactions' dbModel (kBlockIdx, mBlockIdx, transactionIdx + 1)
            pure $ tx : txs
        Left (D.DBError D.KeyNotFound _) -> pure []
        Left err -> do
            L.logError $ show err
            pure []

loadTransactions
    :: D.DBModel
    -> (D.KBlockIdx, D.MBlockIdx)
    -> L.NodeL [D.Transaction]
loadTransactions dbModel (kBlockIdx, mBlockIdx) =
    loadTransactions' dbModel (kBlockIdx, mBlockIdx, 1)

loadMBlocks'
    :: D.DBModel
    -> D.StringHash
    -> (D.KBlockIdx, D.MBlockIdx)
    -> L.NodeL [D.Microblock]
loadMBlocks' dbModel kBlockHash fullIdx@(kBlockIdx, mBlockIdx) = do
    eDBMBlock <- withMBlocksDB dbModel $ L.getValue' @D.MBlockEntity fullIdx
    whenRight eDBMBlock $ \dbMBlock -> L.logDebug
        $  "\n    [" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] MBlock entity"
        <> "\n    " <> show dbMBlock

    case eDBMBlock of
        Right dbMBlock -> do
            txs <- loadTransactions dbModel fullIdx
            let mBlock = D.fromDBMBlock dbMBlock kBlockHash txs
            mBlocks <- loadMBlocks' dbModel kBlockHash (kBlockIdx, mBlockIdx + 1)
            pure $ mBlock : mBlocks
        Left (D.DBError D.KeyNotFound _) -> pure []
        Left err -> do
            L.logError $ show err
            pure []

loadMBlocks :: D.DBModel -> D.StringHash -> D.BlockNumber -> L.NodeL [D.Microblock]
loadMBlocks dbModel kBlockHash kBlockIdx =
    loadMBlocks' dbModel kBlockHash (kBlockIdx, 1)

loadNextKBlock :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock dbModel prevHash = do
    eMbKBlockMeta <- loadKBlockMeta dbModel prevHash
    withResult eMbKBlockMeta $ loadKBlock dbModel

restoreFromDB' :: G.GraphServiceData -> D.StringHash -> L.NodeL ()
restoreFromDB' nodeData kBlockPrevHash = withDBModel nodeData $ \dbModel -> do
    eKBlock <- loadNextKBlock dbModel kBlockPrevHash
    case eKBlock of
        Left (D.DBError D.KeyNotFound _) -> L.logInfo $ "Restoring done: no kBlock with such prev_hash found: " +|| kBlockPrevHash ||+ "."
        Left err                         -> L.logError $ show err
        Right kBlock                     -> do
            let kBlockHash = D.toHash kBlock

            G.acceptKBlock' nodeData kBlock

            mBlocks <- loadMBlocks dbModel kBlockHash (kBlock ^. Lens.number)
            mapM_ (G.acceptMBlock' nodeData) mBlocks
            restoreFromDB' nodeData kBlockHash

restoreFromDB :: G.GraphServiceData -> L.NodeL ()
restoreFromDB nodeData = do
    L.logInfo "Trying to restore from DB..."
    restoreFromDB' nodeData D.genesisHash
