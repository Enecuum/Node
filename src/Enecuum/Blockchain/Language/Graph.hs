module Enecuum.Blockchain.Language.Graph where

import qualified Data.List                          as L
import qualified Data.Map                           as Map

import qualified Enecuum.Blockchain.Domain          as D
import qualified Enecuum.Blockchain.Language.Ledger as L
import qualified Enecuum.Blockchain.Lens            as Lens
import qualified Enecuum.Core.Language              as L
import qualified Enecuum.Core.Types                 as D
import qualified Enecuum.Framework.Domain           as D
import qualified Enecuum.Framework.Language         as L
import           Enecuum.Prelude

-- Bolts and pieces to work with graph

data FindingBlockPredicate = FindByNumber D.BlockNumber

-- | Find a block by the given predicate starting from top and traversing graph down to genesis.
findBlockDownward :: D.WindowedGraph -> FindingBlockPredicate -> D.KBlock -> L.StateL (Maybe D.KBlock)
findBlockDownward wndGraph pred@(FindByNumber targetNumber) currentKBlock = do
    let currentNumber = D._number currentKBlock
    if | currentNumber <  targetNumber -> pure Nothing
       | currentNumber == targetNumber -> pure $ Just currentKBlock
       | currentNumber >  targetNumber -> do
           mbPrevKBlock <- getKBlock wndGraph (D._prevHash currentKBlock)
           case mbPrevKBlock of
               Nothing         -> pure Nothing
               Just prevkBlock -> findBlockDownward wndGraph pred prevkBlock

-- | Get kBlock by Hash.
getKBlock :: D.WindowedGraph -> D.StringHash -> L.StateL (Maybe D.KBlock)
getKBlock wndGraph hash = L.evalGraph (D._graph wndGraph) $ do
    mbKBlock <- L.getNode hash
    case mbKBlock of
        Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure $ Just kBlock
        _                                                                -> pure Nothing

-- | Get Top kBlock.
-- N.B. It assumes the topKBlock hash will always point to the existing KBlock in graph.
getTopKBlock :: D.WindowedGraph -> L.StateL D.KBlock
getTopKBlock wndGraph = do
    topNodeHash <- L.readVar (wndGraph ^. Lens.topKBlock)
    fromJust <$> getKBlock wndGraph topNodeHash

getMBlocksForKBlock :: D.WindowedGraph -> D.StringHash -> L.StateL (Either Text [D.Microblock])
getMBlocksForKBlock wndGraph hash =  L.evalGraph (D._graph wndGraph) $ do
    mbNode <- L.getNode hash
    case mbNode of
        Nothing   -> pure $ Left "KBlock doesn't exist"
        Just node -> Right <$> getMBlocksForKBlock'' node

getMBlocksForKBlock' :: D.WindowedGraph -> D.StringHash -> L.StateL [D.Microblock]
getMBlocksForKBlock' wndGraph hash =  L.evalGraph (D._graph wndGraph) $ do
    mbNode <- L.getNode hash
    case mbNode of
        Nothing   -> pure []
        Just node -> getMBlocksForKBlock'' node

getMBlocksForKBlock'' (D.HNode _ _ _ links _) = do
    mBlocks <- forM (Map.keys links) $ \aNRef -> do
        (D.HNode _ _ (D.fromContent -> block) _ _) <- fromJust <$> L.getNode aNRef
        case block of
            D.MBlockContent mBlock -> pure [mBlock]
            _                      -> pure []
    pure $ join mBlocks

-- Graph scenarios

-- | Add key block to the top of the graph.
-- kBlockSrc is where the KBlock came from. For example, Network or Pending.
addTopKBlock :: Text -> D.BlockchainData -> D.KBlock -> L.StateL Bool
addTopKBlock kBlockSrc bData kBlock = do
    L.logInfo $ "Adding " +| kBlockSrc |+ " KBlock to the graph: " +|| kBlock ||+ "."
    let kBlock' = D.KBlockContent kBlock
    topKBlockHash <- L.readVar (bData ^. Lens.wTopKBlock)

    eMblocks <- getMBlocksForKBlock (bData ^. Lens.windowedGraph) topKBlockHash
    whenRight eMblocks $ \mBlocks -> forM_ mBlocks $ L.calculateLedger bData

    L.evalGraph (bData ^. Lens.wGraph) $ do
        L.newNode kBlock'
        L.newLink topKBlockHash kBlock'

    L.modifyVar (bData ^. Lens.wWindowSize) (+ 1)

    -- change of curNode.
    L.writeVar (bData ^. Lens.wTopKBlock) $ D.toHash kBlock'
    pure True

-- | Add microblock to graph
addMBlock :: D.WindowedGraph -> D.Microblock -> L.StateL Bool
addMBlock wndGraph mblock@(D.Microblock hash _ _ _) = do
    kblock <- getKBlock wndGraph hash

    unless (isJust kblock) $ L.logInfo $ "Can't add MBlock to the graph: KBlock not found (" +|| hash ||+ ")."

    when (isJust kblock) $ do
        L.logInfo $ "Adding MBlock to the graph for KBlock (" +|| hash ||+ ")."
        L.evalGraph (D._graph wndGraph) $ do
            L.newNode (D.MBlockContent mblock)
            L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

-- Return all blocks after given number as a list.
-- Takes a top block and goes down.
-- TODO: WindowedGraph support
findBlocksByNumber :: D.WindowedGraph -> D.BlockNumber -> D.KBlock -> L.StateL [D.KBlock]
findBlocksByNumber wndGraph num currentKBlock = do
    let cNum = D._number currentKBlock
    if  | cNum <  num -> pure []
        | cNum == num -> pure [currentKBlock]
        | cNum >  num -> do
            mbPrevKBlock <- getKBlock wndGraph (D._prevHash currentKBlock)
            case mbPrevKBlock of
                Nothing         -> error "Broken chain" -- TODO: Does not support windowed graph
                Just prevkBlock -> (:) currentKBlock <$> findBlocksByNumber wndGraph num prevkBlock

kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock =
       D._number   kBlock == D._number topKBlock + 1
    && D._prevHash kBlock == D.toHash  topKBlock

-- TODO: this should check whether the KBlock is new or it's duplicated.
kBlockExists :: D.KBlock -> D.KBlock -> Bool
kBlockExists kBlock topKBlock = D._number kBlock <= D._number topKBlock

withKBlockNode' wndGraph onNotFound onNotKBlock action hash = do
    mbKBlock <- L.evalGraph (D._graph wndGraph) $ L.getNode hash
    case mbKBlock of
        Just node | D.isKBlockNode node -> action node
        Just _                          -> onNotKBlock
        Nothing                         -> onNotFound

withKBlockNode wndGraph action hash =
    withKBlockNode' wndGraph (pure ()) (pure ()) action hash

shrinkGraphDownFrom wndGraph node@(D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) links rLinks) = do
    L.logDebug $ "\n    Links: " <> show links
              <> "\n    RLinks: " <> show rLinks

shrinkGraph :: D.BlockchainData -> D.BlockNumber -> D.KBlock -> L.StateL ()
shrinkGraph bData bottomNumber currentKBlock = do
    let wndGraph = bData ^. Lens.windowedGraph
    -- TODO: use more efficient function (don't need list of kBlocks)
    kBlocks <- findBlocksByNumber wndGraph bottomNumber currentKBlock
    -- withKBlockNode wndGraph (pure ()) (pure ()) (shrinkGraphDownFrom wndGraph) bottomHash
    L.logDebug $ "KBlocks down to new bottom: \n" <> show (L.intercalate "\n" (map show kBlocks))
