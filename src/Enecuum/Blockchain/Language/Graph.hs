module Enecuum.Blockchain.Language.Graph where

import qualified Enecuum.Blockchain.Domain          as D
import qualified Enecuum.Blockchain.Language.Ledger as L
import qualified Enecuum.Core.Language              as L
import qualified Enecuum.Core.Types                 as D
import qualified Enecuum.Framework.Domain           as D
import qualified Enecuum.Framework.Language         as L
import qualified Enecuum.Framework.LogState         as Log
import           Enecuum.Prelude

-- | Get kBlock by Hash
getKBlock :: D.StateVar [Text] -> D.BlockchainData -> D.StringHash -> L.StateL (Maybe D.KBlock)
getKBlock logV bData hash = do
    (res, mbMsg) <- L.evalGraph (D._graph bData) $ do
        maybeKBlock <- L.getNode hash
        case maybeKBlock of
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure $ (Just kBlock, Nothing)
            _ -> pure (Nothing, Just $ "KBlock not found by hash: " <> show hash)
    whenJust mbMsg $ Log.stateLog logV
    pure res

-- Get Top kBlock
getTopKeyBlock :: D.StateVar [Text] -> D.BlockchainData -> L.StateL D.KBlock
getTopKeyBlock logV bData = do
    topNodeHash    <- L.readVar $ (D._curNode bData)
    Just topKBlock <- getKBlock logV bData topNodeHash
    pure topKBlock

-- | Add key block to graph
addKBlock :: D.StateVar [Text] -> D.BlockchainData -> D.KBlock -> L.StateL Bool
addKBlock logV bData kBlock = do
    Log.stateLog logV "Adding KBlock to the graph."
    let kBlock' = D.KBlockContent kBlock
    ref <- L.readVar (D._curNode bData)
    L.evalGraph (D._graph bData) $ do
        L.newNode kBlock'
        L.newLink ref kBlock'
    -- change of curNode.
    L.writeVar (D._curNode bData) $ D.toHash kBlock'
    pure True

-- | Add microblock to graph
addMBlock :: D.StateVar [Text] -> D.BlockchainData -> D.Microblock -> L.StateL Bool
addMBlock logV bData mblock@(D.Microblock hash _ _ _) = do
    kblock <- getKBlock logV bData hash

    unless (isJust kblock) $ Log.stateLog logV $ "Can't add MBlock to the graph: KBlock not found (" +|| hash ||+ ")."

    when (isJust kblock) $ do
        Log.stateLog logV $ "Adding MBlock to the graph for KBlock (" +|| hash ||+ ")."
        L.calculateLedger logV bData mblock
        L.evalGraph (D._graph bData) $ do
            L.newNode (D.MBlockContent mblock)
            L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

-- Return all blocks after given number as a list
findBlocksByNumber :: D.StateVar [Text] -> D.BlockchainData -> Integer -> D.KBlock -> L.StateL [D.KBlock]
findBlocksByNumber logV bData num prev =
    let cNum = (D._number prev) in
    if
        | cNum < num -> pure []
        | cNum == num -> pure [prev]
        | cNum > num -> do
            maybeNext <- getKBlock logV bData (D._prevHash prev)
            case maybeNext of
                Nothing   -> error "Broken chain"
                Just next -> (:) prev <$> findBlocksByNumber logV bData num next

kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock =
    (D._number kBlock) == (D._number topKBlock) + 1 && (D._prevHash kBlock) == D.toHash
        (D.KBlockContent topKBlock)
