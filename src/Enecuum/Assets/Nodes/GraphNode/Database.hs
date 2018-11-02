module Enecuum.Assets.Nodes.GraphNode.Database where

import           Enecuum.Prelude

import qualified Enecuum.Domain             as D
import qualified Enecuum.Language           as L
import qualified Enecuum.Framework.LogState as Log
import qualified Enecuum.Blockchain.Lens    as Lens
import qualified Enecuum.Blockchain.DB      as D
import qualified Enecuum.Blockchain.DB.Lens as Lens

import           Enecuum.Config
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Assets.Nodes.GraphNode.Logic as G
import           Enecuum.Assets.Nodes.GraphNode.Config
import qualified Enecuum.Assets.Nodes.CLens as CLens


-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {number:0, nonce: 0, solver: 1}

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (0, "")

-- DB access

withKBlocksDB
    :: forall s a1 db a2
    . (Lens.HasKBlocksDB a1 (D.Storage db), G.HasDb s a1)
    => s -> L.DatabaseL db a2 -> L.NodeL a2
withKBlocksDB nodeData = L.withDatabase (nodeData ^. G.db . Lens.kBlocksDB)

withKBlocksMetaDB
    :: forall s a1 db a2
    . (Lens.HasKBlocksMetaDB a1 (D.Storage db), G.HasDb s a1)
    => s -> L.DatabaseL db a2 -> L.NodeL a2
withKBlocksMetaDB nodeData = L.withDatabase (nodeData ^. G.db . Lens.kBlocksMetaDB)

-- Loading

loadKBlock :: G.GraphNodeData -> D.DBValue D.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock nodeData (D.KBlockMetaValue i) = do
    ePrevHash     <- withKBlocksDB nodeData $ L.getValue' i
    eKBlockEntity <- withKBlocksDB nodeData $ L.getValue' i

    -- Returns Left if any.
    -- TODO: use Data.Validation
    pure $ D.KBlock
        <$> (eKBlockEntity ^. Lens.time')
        <*> (ePrevHash     ^. Lens.prevHash')
        <*> (eKBlockEntity ^. Lens.number')
        <*> (eKBlockEntity ^. Lens.nonce')
        <*> (eKBlockEntity ^. Lens.solver')

loadHashMeta :: G.GraphNodeData -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadHashMeta nodeData hash = withKBlocksMetaDB nodeData $ L.getValue' hash

loadNextKBlock :: G.GraphNodeData -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock nodeData prevHash = do
    eHashMeta <- loadHashMeta nodeData prevHash
    either (pure . Left) (loadKBlock nodeData) eHashMeta

-- Saving

saveKBlock :: G.GraphNodeData -> D.KBlock -> L.NodeL (D.DBResult ())
saveKBlock nodeData kBlock = do
    let k1 = D.toDBKey   @D.KBlockPrevHashEntity kBlock
    let v1 = D.toDBValue @D.KBlockPrevHashEntity kBlock
    let k2 = D.toDBKey   @D.KBlockEntity kBlock
    let v2 = D.toDBValue @D.KBlockEntity kBlock

    L.logInfo $ "[" +|| kBlock ^. Lens.number ||+ "] Saving KBlock (" +|| D.toHash kBlock ||+ "):"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"
    L.logInfo $ "    <" +|| k2 ||+ "> <" +|| v2 ||+ ">"

    eResults <- withKBlocksDB nodeData $ sequence
        [ L.putEntity' @D.KBlockPrevHashEntity kBlock
        , L.putEntity' @D.KBlockEntity         kBlock
        ]

    -- Returns Left if any.
    -- TODO: use Data.Validation
    pure $ foldr (\a b -> a >>= const b) (Right ()) eResults

saveKBlockMeta :: G.GraphNodeData -> D.KBlock -> L.NodeL (D.DBResult ())
saveKBlockMeta nodeData kBlock = do
    let k1 = D.toDBKey   @D.KBlockMetaEntity kBlock
    let v1 = D.toDBValue @D.KBlockMetaEntity kBlock

    L.logInfo $ "[" +|| kBlock ^. Lens.number ||+ "] Saving KBlock meta:"
    L.logInfo $ "    <" +|| k1 ||+ "> <" +|| v1 ||+ ">"
    
    withKBlocksMetaDB nodeData $ L.putEntity' @D.KBlockMetaEntity kBlock

-- Interface

restoreFromDB :: G.GraphNodeData -> L.NodeL ()
restoreFromDB nodeData = do
    L.logInfo "Trying to restore from DB..."
    eKBlock <- loadNextKBlock nodeData D.genesisHash
    case eKBlock of
        Left err     -> L.logError $ show err
        Right kBlock -> G.acceptKBlock' nodeData kBlock

dumpToDB' :: G.GraphNodeData -> D.KBlock -> L.NodeL ()
dumpToDB' nodeData kBlock = do

    eResults <- sequence 
        [ saveKBlock     nodeData kBlock
        , saveKBlockMeta nodeData kBlock
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