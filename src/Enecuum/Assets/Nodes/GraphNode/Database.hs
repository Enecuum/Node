module Enecuum.Assets.Nodes.GraphNode.Database where

import           Enecuum.Prelude

import qualified Enecuum.Domain             as D
import qualified Enecuum.Language           as L
import qualified Enecuum.Framework.LogState as Log
import qualified Enecuum.Blockchain.Lens    as Lens
import qualified Enecuum.Blockchain.DB      as D
import qualified Enecuum.Blockchain.DB.Lens as Lens

import qualified Enecuum.Assets.Nodes.GraphNode.Logic as G


-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {number:0, nonce: 0, solver: 1}

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (0, "")

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

-- Loading

loadKBlock :: D.DBModel -> D.DBValue D.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock dbModel (D.KBlockMetaValue i) = do
    L.logInfo "Loading KBlock prev hash..."
    ePrevHash     <- withKBlocksDB dbModel $ L.getValue' i
    L.logInfo "Loading KBlock entity..."
    eKBlockEntity <- withKBlocksDB dbModel $ L.getValue' i

    -- Returns Left if any.
    -- TODO: use Data.Validation
    pure $ D.KBlock
        <$> (eKBlockEntity ^. Lens.time')
        <*> (ePrevHash     ^. Lens.prevHash')
        <*> (eKBlockEntity ^. Lens.number')
        <*> (eKBlockEntity ^. Lens.nonce')
        <*> (eKBlockEntity ^. Lens.solver')

loadHashMeta :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadHashMeta dbModel hash = do
    L.logInfo "Loading hash meta..."
    withKBlocksMetaDB dbModel $ L.getValue' hash

loadNextKBlock :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock dbModel prevHash = do
    eHashMeta <- loadHashMeta dbModel prevHash
    either (pure . Left) (loadKBlock dbModel) eHashMeta

-- Saving

saveKBlock :: D.DBModel -> D.KBlock -> L.NodeL (D.DBResult ())
saveKBlock dbModel kBlock = do
    let k1 = D.toDBKey   @D.KBlockPrevHashEntity kBlock
    let v1 = D.toDBValue @D.KBlockPrevHashEntity kBlock
    let k2 = D.toDBKey   @D.KBlockEntity kBlock
    let v2 = D.toDBValue @D.KBlockEntity kBlock

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
    let k1 = D.toDBKey   @D.KBlockMetaEntity kBlock
    let v1 = D.toDBValue @D.KBlockMetaEntity kBlock

    L.logInfo $ "[" +|| kBlock ^. Lens.number ||+ "] Saving KBlock meta:"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"
    
    withKBlocksMetaDB dbModel $ L.putEntity' @D.KBlockMetaEntity kBlock

-- Interface

withDBModel :: G.GraphNodeData -> (D.DBModel -> L.NodeL ()) -> L.NodeL ()
withDBModel nodeData act = case nodeData ^. G.db of
    Nothing      -> pure ()
    Just dbModel -> act dbModel

restoreFromDB :: G.GraphNodeData -> L.NodeL ()
restoreFromDB nodeData = withDBModel nodeData $ \dbModel -> do
    L.logInfo "Trying to restore from DB..."
    eKBlock <- loadNextKBlock dbModel D.genesisHash
    case eKBlock of
        Left err     -> L.logError $ show err
        Right kBlock -> G.acceptKBlock' nodeData kBlock

dumpToDB' :: G.GraphNodeData -> D.KBlock -> L.NodeL ()
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
            stateVar <- L.newVarIO []
            mbPrevKBlock <- L.atomically $ L.getKBlock stateVar (nodeData ^. G.blockchain) (kBlock ^. Lens.prevHash)
            Log.writeLog stateVar
            case mbPrevKBlock of
                Nothing         -> L.logError $ "Prev KBlock not found in graph: " +|| (kBlock ^. Lens.prevHash) ||+ "."
                Just prevKBlock -> dumpToDB' nodeData prevKBlock

dumpToDB :: G.GraphNodeData -> L.NodeL ()
dumpToDB nodeData = do
    L.logInfo "Dumping to DB..."
    stateVar <- L.newVarIO []
    topKBlock <- L.atomically $ L.getTopKeyBlock stateVar (nodeData ^. G.blockchain)
    Log.writeLog stateVar
    dumpToDB' nodeData topKBlock