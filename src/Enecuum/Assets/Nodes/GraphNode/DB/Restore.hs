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

loadMBlocks' :: D.DBModel -> D.StringHash -> D.BlockNumber -> L.NodeL (D.DBResult [D.Microblock])
loadMBlocks' dbModel kBlockHash kBlockIdx = do
    let mBlocksLoader = [L.getValue' @D.MBlockEntity (kBlockIdx, n :: D.BlockNumber) | n <- [1..]]
    eResults <- withMBlocksDB dbModel $ materialize mBlocksLoader
    pure $ map toMBlock <$> eResults
    where
        toMBlock (D.MBlockValue publisher signature) = D.Microblock
          { D._keyBlock     = kBlockHash
          , D._transactions = []          -- TODO
          , D._publisher    = publisher
          , D._signature    = signature
          }

loadKBlockHashMeta :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult (D.DBValue D.KBlockMetaEntity))
loadKBlockHashMeta dbModel hash = withKBlocksMetaDB dbModel $ L.getValue' hash

loadNextKBlock :: D.DBModel -> D.StringHash -> L.NodeL (D.DBResult D.KBlock)
loadNextKBlock dbModel prevHash = do
    eMbHashMeta <- loadKBlockHashMeta dbModel prevHash
    withResult eMbHashMeta $ \hashMeta -> loadKBlock dbModel hashMeta

loadMBlocks :: D.DBModel -> D.StringHash -> D.BlockNumber -> L.NodeL (D.DBResult [D.Microblock])
loadMBlocks = loadMBlocks'


restoreFromDB' :: G.GraphNodeData -> D.StringHash -> L.NodeL ()
restoreFromDB' nodeData kBlockHash = withDBModel nodeData $ \dbModel -> do
    eKBlock <- loadNextKBlock dbModel kBlockHash
    case eKBlock of
        Left (D.DBError D.KeyNotFound _) -> L.logInfo $ "Restoring done: no kBlock with such prev_hash found: " +|| kBlockHash ||+ "."
        Left err                         -> L.logError $ show err
        Right kBlock                     -> do
            L.logInfo $ "KBlock loaded: " +|| kBlock ||+ "."
            mBlocks <- loadMBlocks dbModel kBlockHash (kBlock ^. Lens.number)
            G.acceptKBlock' nodeData kBlock
            restoreFromDB' nodeData $ D.toHash kBlock

restoreFromDB :: G.GraphNodeData -> L.NodeL ()
restoreFromDB nodeData = do
    L.logInfo "Trying to restore from DB..."
    restoreFromDB' nodeData D.genesisHash
