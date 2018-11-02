module Enecuum.Assets.Nodes.GraphNode.Database where

import           Enecuum.Prelude
import           Control.Lens (Getter, to)

import qualified Enecuum.Domain          as D
import qualified Enecuum.Language        as L
import qualified Enecuum.Blockchain.Lens as Lens

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
-- AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (0, "")
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (1, "")

time' :: Getter (D.DBResult (D.DBValue D.KBlockEntity)) (D.DBResult D.Time')
time' = to (\eVal -> eVal >>= (\(D.KBlockValue t _ _ _) -> Right t))

number' :: Getter (D.DBResult (D.DBValue D.KBlockEntity)) (D.DBResult D.Number)
number' = to (\eVal -> eVal >>= (\(D.KBlockValue _ n _ _) -> Right n))

nonce' :: Getter (D.DBResult (D.DBValue D.KBlockEntity)) (D.DBResult D.Nonce)
nonce' = to (\eVal -> eVal >>= (\(D.KBlockValue _ _ n _) -> Right n))

solver' :: Getter (D.DBResult (D.DBValue D.KBlockEntity)) (D.DBResult D.Solver)
solver' = to (\eVal -> eVal >>= (\(D.KBlockValue _ _ _ s) -> Right s))

prevHash' :: Getter (D.DBResult (D.DBValue D.KBlockPrevHashEntity)) (D.DBResult D.PrevHash)
prevHash' = to (\eVal -> eVal >>= (\(D.KBlockPrevHashValue ph) -> Right ph))

loadKBlock :: G.GraphNodeData -> D.DBValue D.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock nodeData (D.KBlockMetaValue i) = do
    ePrevHash <- L.withDatabase (nodeData ^. G.db . Lens.kBlocksDB)
        $ L.getValue $ D.toDBKey i
    eKBlockEntity <- L.withDatabase (nodeData ^. G.db . Lens.kBlocksDB)
        $ L.getValue @D.KBlockEntity $ D.toDBKey i

    pure $ D.KBlock
        <$> (eKBlockEntity ^. time')
        <*> (ePrevHash     ^. prevHash')
        <*> (eKBlockEntity ^. number')
        <*> (eKBlockEntity ^. nonce')
        <*> (eKBlockEntity ^. solver')


loadHashMeta :: G.GraphNodeData -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadHashMeta nodeData hash = L.withDatabase (nodeData ^. G.db . Lens.kBlocksMetaDB)
    $ L.getValue $ D.toDBKey hash

loadNextKBlock :: G.GraphNodeData -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock nodeData prevHash = do
    eHashMeta <- loadHashMeta nodeData prevHash
    either (pure . Left) (loadKBlock nodeData) eHashMeta

restoreFromDB :: G.GraphNodeData -> L.NodeL ()
restoreFromDB nodeData = do
    eKBlock <- loadNextKBlock nodeData D.genesisIndicationHash
    case eKBlock of
        Left err     -> pure ()      -- TODO: message (it not necessary an error)
        Right kBlock -> G.acceptKBlock' nodeData kBlock