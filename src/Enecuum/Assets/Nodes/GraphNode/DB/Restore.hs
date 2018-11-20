{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.GraphNode.DB.Restore where

import           Enecuum.Prelude

import qualified Enecuum.Blockchain.DB                     as D
import qualified Enecuum.Blockchain.DB.Lens                as Lens
import qualified Enecuum.Blockchain.Lens                   as Lens
import qualified Enecuum.Domain                            as D
import qualified Enecuum.Language                          as L

import           Enecuum.Assets.Nodes.GraphNode.DB.Helpers
import qualified Enecuum.Assets.Nodes.GraphNode.Logic      as G

loadKBlock :: D.DBModel -> D.DBValue D.KBlockMetaEntity -> L.NodeL (D.DBResult D.KBlock)
loadKBlock dbModel (D.KBlockMetaValue i) = do
    ePrevHash     <- withKBlocksDB dbModel $ L.getValue' @D.KBlockPrevHashEntity i
    eKBlockEntity <- withKBlocksDB dbModel $ L.getValue' @D.KBlockEntity i

    pure $ D.KBlock
        <$> (eKBlockEntity ^. Lens.time'    )
        <*> (ePrevHash     ^. Lens.prevHash')
        <*> (eKBlockEntity ^. Lens.number'  )
        <*> (eKBlockEntity ^. Lens.nonce'   )
        <*> (eKBlockEntity ^. Lens.solver'  )

fromDBMBlock (D.MBlockValue publisher signature) kBlockHash txs = D.Microblock
    { D._keyBlock     = kBlockHash
    , D._transactions = txs
    , D._publisher    = publisher
    , D._signature    = signature
    }

-- We don't know how many mblocks exists. We also don't know their indexes,
-- But we know the indexes are counting from 1 to N.
-- We will be loading them all untill the KeyNotFound is reached.
loadMBlocks' mBlockIdx dbModel kBlockHash kBlockIdx = do
    eDBMBlock <- withMBlocksDB dbModel $ L.getValue' @D.MBlockEntity (kBlockIdx, mBlockIdx)
    case eDBMBlock of
        Right dbMBlock -> do
            txs <- loadTransactions dbModel kBlockIdx mBlockIdx
            let mBlock = fromDBMBlock dbMBlock kBlockHash txs
            mBlocks <- loadMBlocks' (mBlockIdx + 1) dbModel kBlockHash kBlockIdx
            pure $ mBlock : mBlocks
        Left (D.DBError D.KeyNotFound _) -> pure []
        Left err -> do
            L.logError $ show err
            pure []

loadMBlocks :: D.DBModel -> D.StringHash -> D.BlockNumber -> L.NodeL (D.DBResult [D.Microblock])
loadMBlocks dbModel = loadMBlocks' 1

loadKBlockHashMeta :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadKBlockHashMeta dbModel hash = withKBlocksMetaDB dbModel $ L.getValue' hash

loadNextKBlock :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock dbModel prevHash = do
    eMbHashMeta <- loadKBlockHashMeta dbModel prevHash
    withResult eMbHashMeta $ \hashMeta -> loadKBlock dbModel hashMeta

restoreFromDB' :: G.GraphNodeData -> D.StringHash -> L.NodeL ()
restoreFromDB' nodeData kBlockHash = withDBModel nodeData $ \dbModel -> do
    eKBlock <- loadNextKBlock dbModel kBlockHash
    case eKBlock of
        Left (D.DBError D.KeyNotFound _) -> L.logInfo $ "Restoring done: no kBlock with such prev_hash found: " +|| kBlockHash ||+ "."
        Left err                         -> L.logError $ show err
        Right kBlock                     -> do
            L.logInfo $ "KBlock loaded: " +|| kBlock ||+ "."
            G.acceptKBlock' nodeData kBlock

            mBlocks <- loadMBlocks dbModel kBlockHash (kBlock ^. Lens.number)
            mapM_ (G.acceptMBlock' nodeData) mBlocks
            restoreFromDB' nodeData $ D.toHash kBlock

restoreFromDB :: G.GraphNodeData -> L.NodeL ()
restoreFromDB nodeData = do
    L.logInfo "Trying to restore from DB..."
    restoreFromDB' nodeData D.genesisHash
