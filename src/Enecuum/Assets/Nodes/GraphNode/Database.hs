module Enecuum.Assets.Nodes.GraphNode.Database where

import           Enecuum.Prelude

import qualified Enecuum.Domain             as D
import qualified Enecuum.Language           as L
import qualified Enecuum.Blockchain.Lens    as Lens
import qualified Enecuum.Blockchain.DB      as D
import qualified Enecuum.Blockchain.DB.Lens as Lens

import qualified Enecuum.Assets.Nodes.GraphNode.Logic as G


-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {time:0, number:0, nonce: 0, solver: 1}

-- 0000001|0 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY=
-- 0000001|1 {time:1, number:1, nonce: 1, solver: 1}

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (1, "")

-- DB access

withKBlocksDB
    :: forall s db a
    .  Lens.HasKBlocksDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withKBlocksDB dbModel = L.withDatabase (dbModel ^. Lens.kBlocksDB)

withKBlocksMetaDB
    :: forall s db a
    .  Lens.HasKBlocksMetaDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withKBlocksMetaDB dbModel = L.withDatabase (dbModel ^. Lens.kBlocksMetaDB)


-- TODO: new errors handling approach. It becomes too complicated.

withResult
    :: Applicative f
    => Either a t
    -> (t -> f (Either a b))
    -> f (Either a b)
withResult (Left err)     _      = pure $ Left err
withResult (Right result) action = action result

-- Loading

loadKBlock :: D.DBModel -> D.DBValue D.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock dbModel (D.KBlockMetaValue i) = do
    ePrevHash     <- withKBlocksDB dbModel $ L.getValue' @D.KBlockPrevHashEntity i
    eKBlockEntity <- withKBlocksDB dbModel $ L.getValue' @D.KBlockEntity i

    -- Returns Left if any.
    -- TODO: use Data.Validation
    pure $ D.KBlock
        <$> (eKBlockEntity ^. Lens.time'    )
        <*> (ePrevHash     ^. Lens.prevHash')
        <*> (eKBlockEntity ^. Lens.number'  )
        <*> (eKBlockEntity ^. Lens.nonce'   )
        <*> (eKBlockEntity ^. Lens.solver'  )

loadHashMeta :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadHashMeta dbModel hash = withKBlocksMetaDB dbModel $ L.getValue' hash

loadNextKBlock :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock dbModel prevHash = do
    eMbHashMeta <- loadHashMeta dbModel prevHash
    withResult eMbHashMeta $ \hashMeta -> loadKBlock dbModel hashMeta

-- Saving

saveKBlock :: D.DBModel -> D.KBlock -> L.NodeL (D.DBResult ())
saveKBlock dbModel kBlock = do
    let (k1, v1) = D.toDBEntity @D.KBlockPrevHashEntity kBlock
    let (k2, v2) = D.toDBEntity @D.KBlockEntity         kBlock

    L.logInfo $ "[" +|| kBlock ^. Lens.number ||+ "] Saving KBlock (" +|| D.toHash kBlock ||+ "):"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"
    L.logInfo $ "    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    eResults <- withKBlocksDB dbModel $ sequence
        [ L.putEntity' @D.KBlockPrevHashEntity kBlock
        , L.putEntity' @D.KBlockEntity         kBlock
        ]

    -- Returns Left if any.
    -- TODO: use Data.Validation
    pure $ foldr (\a b -> a >>= const b) (Right ()) eResults

saveKBlockMeta :: D.DBModel -> D.KBlock -> L.NodeL (D.DBResult ())
saveKBlockMeta dbModel kBlock = do
    let (k1, v1) = D.toDBEntity @D.KBlockMetaEntity kBlock

    L.logInfo $ "[" +|| kBlock ^. Lens.number ||+ "] Saving KBlock meta:"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"

    withKBlocksMetaDB dbModel $ L.putEntity' @D.KBlockMetaEntity kBlock

-- Interface

withDBModel :: (G.GraphNodeData' node) -> (D.DBModel -> L.NodeL ()) -> L.NodeL ()
withDBModel nodeData act = case nodeData ^. G.db of
    Nothing      -> pure ()
    Just dbModel -> act dbModel

restoreFromDB' :: (G.GraphNodeData' node) -> D.StringHash -> L.NodeL ()
restoreFromDB' nodeData kBlockHash = withDBModel nodeData $ \dbModel -> do
    eKBlock <- loadNextKBlock dbModel kBlockHash
    case eKBlock of
        Left (D.DBError D.KeyNotFound _) -> L.logInfo $ "Restoring done: no kBlock with such prev_hash found: " +|| kBlockHash ||+ "."
        Left err                         -> L.logError $ show err
        Right kBlock                     -> do
            L.logInfo $ "KBlock loaded: " +|| kBlock ||+ "."
            G.acceptKBlock' nodeData kBlock
            restoreFromDB' nodeData $ D.toHash kBlock

restoreFromDB :: (G.GraphNodeData' node) -> L.NodeL ()
restoreFromDB nodeData = do
    L.logInfo "Trying to restore from DB..."
    restoreFromDB' nodeData D.genesisHash

dumpToDB' :: (G.GraphNodeData' node) -> D.KBlock -> L.NodeL ()
dumpToDB' nodeData kBlock = withDBModel nodeData $ \dbModel -> do

    eResults <- sequence
        [ saveKBlock     dbModel kBlock
        , saveKBlockMeta dbModel kBlock
        ]

    -- Returns Left if any.
    -- TODO: use Data.Validation
    let eResult = foldr (\a b -> a >>= const b) (Right ()) eResults
    case eResult of
        Right _ | kBlock ^. Lens.prevHash == D.genesisIndicationHash -> L.logInfo "Dumping done: Genesis reached."
        Left err -> L.logError $ show err
        _ -> do
            mbPrevKBlock <- L.atomically $ L.getKBlock (nodeData ^. G.blockchain) (kBlock ^. Lens.prevHash)
            case mbPrevKBlock of
                Nothing         -> L.logError $ "Prev KBlock not found in graph: " +|| kBlock ^. Lens.prevHash ||+ "."
                Just prevKBlock -> dumpToDB' nodeData prevKBlock

dumpToDB :: (G.GraphNodeData' node) -> L.NodeL ()
dumpToDB nodeData = do
    L.logInfo "Dumping to DB..."
    topKBlock <- L.atomically $ L.getTopKeyBlock (nodeData ^. G.blockchain)
    dumpToDB' nodeData topKBlock
