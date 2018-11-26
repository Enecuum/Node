module Enecuum.Assets.Nodes.GraphNode.DB.Dump where

import           Enecuum.Prelude

import qualified Enecuum.Blockchain.DB                           as D
import qualified Enecuum.Blockchain.DB.Lens                      as Lens
import qualified Enecuum.Blockchain.Lens                         as Lens
import qualified Enecuum.Domain                                  as D
import qualified Enecuum.Language                                as L

import           Enecuum.Assets.Nodes.GraphNode.DB.Helpers
import qualified Enecuum.Assets.Nodes.GraphNode.GraphServiceData as G
import qualified Enecuum.Assets.Nodes.GraphNode.Logic            as G

saveKBlock :: D.DBModel -> D.KBlock -> L.NodeL (D.DBResult ())
saveKBlock dbModel kBlock = do
    let kBlockIdx = kBlock ^. Lens.number

    let (k1, v1) = D.toDBEntity @D.KBlockPrevHashEntity kBlock
    let (k2, v2) = D.toDBEntity @D.KBlockEntity         kBlock
    let (k3, v3) = D.toDBEntity @D.KBlockMetaEntity     kBlock

    L.logDebug
          $  "\n    [" +|| kBlockIdx ||+ "] KBlock (" +|| D.toHash kBlock ||+ ")"
          <> "\n    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"
          <> "\n    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    L.logDebug
          $  "\n    [" +|| kBlockIdx ||+ "] KBlock meta"
          <> "\n    <" +|| k3 ||+ "> <" +|| v3 ||+ ">"

    eResults <- sequence
        [ withKBlocksDB     dbModel $ L.putEntity' @D.KBlockPrevHashEntity kBlock
        , withKBlocksDB     dbModel $ L.putEntity' @D.KBlockEntity         kBlock
        , withKBlocksMetaDB dbModel $ L.putEntity' @D.KBlockMetaEntity     kBlock
        ]
    pure $ fmap (const ()) $ sequence eResults

saveMBlock' :: D.DBModel -> D.KBlockIdx -> (D.MBlockIdx, D.Microblock) -> L.NodeL (D.DBResult ())
saveMBlock' dbModel kBlockIdx (mBlockIdx, mBlock) = do
    let k1 = D.toDBKey   @D.MBlockEntity (kBlockIdx, mBlockIdx)
    let v1 = D.toDBValue @D.MBlockEntity mBlock

    let k2 = D.toDBKey   @D.MBlockMetaEntity mBlock
    let v2 = D.toDBValue @D.MBlockMetaEntity (kBlockIdx, mBlockIdx)

    L.logDebug
        $  "\n    [" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] MBlock (" +|| D.toHash mBlock ||+ ")"
        <> "\n    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"

    L.logDebug
        $  "\n    [" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] MBlockMeta"
        <> "\n    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    eResults <- sequence
        [ withMBlocksDB     dbModel $ L.putEntity k1 v1
        , withMBlocksMetaDB dbModel $ L.putEntity k2 v2
        ]
    pure $ fmap (const ()) $ sequence eResults

saveMBlock :: D.DBModel -> D.KBlockIdx -> (D.MBlockIdx, D.Microblock) -> L.NodeL (D.DBResult ())
saveMBlock dbModel kBlockIdx (mBlockIdx, mBlock) = do
    let txs = mBlock ^. Lens.transactions
    eMBlockSaved <- saveMBlock' dbModel kBlockIdx (mBlockIdx, mBlock)
    eTransactionsSaved' <- mapM (saveTransaction dbModel kBlockIdx mBlockIdx) $ zip [1..] txs
    pure $ fmap (const ()) $ sequence $ eMBlockSaved : eTransactionsSaved'

saveTransaction :: D.DBModel -> D.KBlockIdx -> D.MBlockIdx -> (D.TransactionIdx, D.Transaction) -> L.NodeL (D.DBResult ())
saveTransaction dbModel kBlockIdx mBlockIdx (transactionIdx, tx) = do
    let k1 = D.toDBKey   @D.TransactionEntity (kBlockIdx, mBlockIdx, transactionIdx)
    let v1 = D.toDBValue @D.TransactionEntity tx

    let k2 = D.toDBKey   @D.TransactionMetaEntity tx
    let v2 = D.toDBValue @D.TransactionMetaEntity (kBlockIdx, mBlockIdx, transactionIdx)

    L.logDebug
        $  "\n    [" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] [" +|| transactionIdx ||+ "] Transaction"
        <> "\n    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"

    L.logDebug
        $  "\n    [" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] [" +|| transactionIdx ||+ "] TransactionMeta"
        <> "\n    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    eResults <- sequence
        [ withTransactionsDB     dbModel $ L.putEntity k1 v1
        , withTransactionsMetaDB dbModel $ L.putEntity k2 v2
        ]
    pure $ fmap (const ()) $ sequence eResults

dumpToDB' :: G.GraphServiceData -> D.KBlock -> L.NodeL ()
dumpToDB' nodeData kBlock = withDBModel nodeData $ \dbModel -> do

    let kBlockPrevHash = kBlock ^. Lens.prevHash
    let kBlockIdx      = kBlock ^. Lens.number
    let kBlockHash     = D.toHash kBlock
    let genesisReached = kBlockPrevHash == D.genesisIndicationHash

    let bData = nodeData ^. G.blockchain
    let wndGraph = bData ^. Lens.windowedGraph

    mBlocks         <- L.atomically $ L.getMBlocksForKBlock' wndGraph kBlockHash

    eKBlockResult   <- saveKBlock dbModel kBlock
    eMBlocksResults <- mapM (saveMBlock dbModel kBlockIdx) $ zip [1..] mBlocks

    case sequence (eKBlockResult : eMBlocksResults) of
        Right _ | genesisReached -> L.logInfo "Dumping done: Genesis reached."
        Left err                 -> L.logError $ show err
        _ -> do
            mbPrevKBlock <- L.atomically $ L.getKBlock wndGraph kBlockPrevHash
            case mbPrevKBlock of
                Nothing         -> L.logError $ "Prev KBlock not found in graph: " +|| kBlockPrevHash ||+ "."
                Just prevKBlock -> dumpToDB' nodeData prevKBlock

dumpToDB :: G.GraphServiceData -> L.NodeL ()
dumpToDB nodeData = do
    L.logInfo "Dumping to DB..."
    topKBlock <- L.atomically $ L.getTopKBlock $ nodeData ^. G.blockchain . Lens.windowedGraph
    dumpToDB' nodeData topKBlock
