module Enecuum.Assets.Nodes.GraphNode.DB.Dump where

import           Enecuum.Prelude

import qualified Enecuum.Blockchain.DB                     as D
import qualified Enecuum.Blockchain.DB.Lens                as Lens
import qualified Enecuum.Blockchain.Lens                   as Lens
import qualified Enecuum.Domain                            as D
import qualified Enecuum.Language                          as L

import           Enecuum.Assets.Nodes.GraphNode.DB.Helpers
import qualified Enecuum.Assets.Nodes.GraphNode.Logic      as G

saveKBlock :: D.DBModel -> D.KBlock -> L.NodeL [D.DBResult ()]
saveKBlock dbModel kBlock = do
    let kBlockIdx = kBlock ^. Lens.number

    let (k1, v1) = D.toDBEntity @D.KBlockPrevHashEntity kBlock
    let (k2, v2) = D.toDBEntity @D.KBlockEntity         kBlock
    let (k3, v3) = D.toDBEntity @D.KBlockMetaEntity     kBlock

    L.logInfo $ "[" +|| kBlockIdx ||+ "] Saving KBlock (" +|| D.toHash kBlock ||+ "):"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"
    L.logInfo $ "    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    L.logInfo $ "[" +|| kBlockIdx ||+ "] Saving KBlock meta:"
    L.logInfo $ "    <" +|| k3 ||+ "> <" +|| v3 ||+ ">"

    sequence
        [ withKBlocksDB     dbModel $ L.putEntity' @D.KBlockPrevHashEntity kBlock
        , withKBlocksDB     dbModel $ L.putEntity' @D.KBlockEntity         kBlock
        , withKBlocksMetaDB dbModel $ L.putEntity' @D.KBlockMetaEntity kBlock
        ]

saveMBlock :: D.DBModel -> D.KBlockIdx -> (D.MBlockIdx, D.Microblock) -> L.NodeL [D.DBResult ()]
saveMBlock dbModel kBlockIdx mBlockData@(mBlockIdx, mBlock) = do
    let k1 = D.toDBKey   @D.MBlockEntity (kBlockIdx, mBlockIdx)
    let v1 = D.toDBValue @D.MBlockEntity mBlock

    let k2 = D.toDBKey   @D.MBlockMetaEntity mBlock
    let v2 = D.toDBValue @D.MBlockMetaEntity (kBlockIdx, mBlockIdx)

    L.logInfo $ "[" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] Saving MBlock (" +|| D.toHash mBlock ||+ "):"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"

    L.logInfo $ "[" +|| kBlockIdx ||+ "] [" +|| mBlockIdx ||+ "] Saving MBlockMeta (" +|| D.toHash mBlock ||+ "):"
    L.logInfo $ "    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    sequence
        [ withMBlocksDB     dbModel $ L.putEntity k1 v1
        , withMBlocksMetaDB dbModel $ L.putEntity k2 v2
        ]

dumpToDB' :: G.GraphNodeData -> D.KBlock -> L.NodeL ()
dumpToDB' nodeData kBlock = withDBModel nodeData $ \dbModel -> do

    let kBlockPrevHash = kBlock ^. Lens.prevHash
    let kBlockIdx      = kBlock ^. Lens.number
    let kBlockHash     = D.toHash kBlock
    let genesisReached = kBlockPrevHash == D.genesisIndicationHash

    mBlocks         <- L.atomically $ L.getMBlocksForKBlock' (nodeData ^. G.blockchain) kBlockHash
    eKBlockResults  <- saveKBlock dbModel kBlock
    eMBlocksResults <- mapM (saveMBlock dbModel kBlockIdx) $ zip [1..] mBlocks

    let eResults = eKBlockResults <> join eMBlocksResults

    case sequence eResults of
        Right _ | genesisReached -> L.logInfo "Dumping done: Genesis reached."
        Left err                 -> L.logError $ show err
        _ -> do
            mbPrevKBlock <- L.atomically $ L.getKBlock (nodeData ^. G.blockchain) kBlockPrevHash
            case mbPrevKBlock of
                Nothing         -> L.logError $ "Prev KBlock not found in graph: " +|| kBlockPrevHash ||+ "."
                Just prevKBlock -> dumpToDB' nodeData prevKBlock

dumpToDB :: G.GraphNodeData -> L.NodeL ()
dumpToDB nodeData = do
    L.logInfo "Dumping to DB..."
    topKBlock <- L.atomically $ L.getTopKeyBlock (nodeData ^. G.blockchain)
    dumpToDB' nodeData topKBlock
