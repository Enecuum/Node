module Enecuum.Blockchain.Language.Graph where

import qualified Data.List                          as L
import qualified Data.Map                           as Map

import qualified Enecuum.Blockchain.Domain          as D
import qualified Enecuum.Blockchain.Language.Ledger as L
import qualified Enecuum.Blockchain.Lens            as Lens
import qualified Enecuum.Core.Language              as L
import qualified Enecuum.Core.Lens                  as Lens
import qualified Enecuum.Core.Types                 as D
import qualified Enecuum.Framework.Domain           as D
import qualified Enecuum.Framework.Language         as L
import           Enecuum.Prelude

-- Bolts and pieces to work with graph
type KNode = (D.KBlock, D.GraphNode)

-- Predicate to search nodes
data FindingBlockPredicate = FindByNumber D.BlockNumber

-- | Get kBlock & node by Hash.
getKBlockNode :: D.WindowedGraph -> D.StringHash -> L.StateL (Maybe KNode)
getKBlockNode wndGraph hash = L.evalGraph (D._graph wndGraph) $ do
    mbNode <- L.getNode hash
    case mbNode of
        Just node@(D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure $ Just (kBlock, node)
        _                                                                     -> pure Nothing

-- | Get kBlock by Hash.
getKBlock :: D.WindowedGraph -> D.StringHash -> L.StateL (Maybe D.KBlock)
getKBlock wndGraph hash = fst <<$>> getKBlockNode wndGraph hash

findKBlockNode :: D.WindowedGraph -> [D.StringHash] -> L.StateL (Maybe KNode)
findKBlockNode _ [] = pure Nothing
findKBlockNode wndGraph (rLink : rLinks) = getKBlockNode wndGraph rLink >>= \case
    Nothing    -> findKBlockNode wndGraph rLinks
    Just kNode -> pure $ Just kNode

-- | Find a KBlock by the given predicate starting from top and traversing graph down to genesis.
-- Returns KBlock & node.
findKBlockNodeDownward'
    :: D.WindowedGraph
    -> FindingBlockPredicate
    -> Maybe KNode
    -> L.StateL (Maybe KNode)
findKBlockNodeDownward' wndGraph predicate@(FindByNumber targetNumber) Nothing = pure Nothing
findKBlockNodeDownward' wndGraph predicate@(FindByNumber targetNumber) (Just current) = do
    let (currentKBlock, currentNode) = current
    let currentNumber = D._number currentKBlock
    if | currentNumber <  targetNumber -> pure Nothing
       | currentNumber == targetNumber -> pure $ Just current
       | currentNumber >  targetNumber -> do
           mbPrev <- getKBlockNode wndGraph (D._prevHash currentKBlock)
           findKBlockNodeDownward' wndGraph predicate mbPrev

-- | Find a KBlock by the given predicate starting from top and traversing graph down to genesis.
-- Returns KBlock & node.
findKBlockNodeDownward
    :: D.WindowedGraph
    -> FindingBlockPredicate
    -> D.StringHash
    -> L.StateL (Maybe KNode)
findKBlockNodeDownward wndGraph predicate kBlockHash = do
    mbNode <- getKBlockNode wndGraph kBlockHash
    findKBlockNodeDownward' wndGraph predicate mbNode

-- | Returns all nodes and links that pointing to this node.
getPredecessors :: D.WindowedGraph -> D.GraphNode -> L.StateL ([D.GraphNode], [D.StringHash])
getPredecessors wndGraph node = do
    let (D.HNode _ _ _ _ (D.hashLinks -> rLinks)) = node
    predNodes <- L.evalGraph (D._graph wndGraph) $ catMaybes <$> mapM L.getNode rLinks
    pure (predNodes, rLinks)

-- Return all blocks after given number as a list.
-- Takes a top block and goes down.
-- TODO: WindowedGraph support
-- TODO: merge with findBlockDownward
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

-- | Get Top kBlock.
-- N.B. It assumes the topKBlock hash will always point to the existing KBlock in graph.
getTopKBlock :: D.WindowedGraph -> L.StateL D.KBlock
getTopKBlock wndGraph = do
    topNodeHash <- L.readVar (wndGraph ^. Lens.topKBlockHash)
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

kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock =
       D._number   kBlock == D._number topKBlock + 1
    && D._prevHash kBlock == D.toHash  topKBlock

-- TODO: this should check whether the KBlock is new or it's duplicated.
kBlockExists :: D.KBlock -> D.KBlock -> Bool
kBlockExists kBlock topKBlock = D._number kBlock <= D._number topKBlock

-- Graph scenarios

-- | Add key block to the top of the graph.
-- kBlockSrc is where the KBlock came from. For example, Network or Pending.
addTopKBlock :: Text -> D.BlockchainData -> D.KBlock -> L.StateL Bool
addTopKBlock kBlockSrc bData kBlock = do
    L.logInfo $ "Adding " +| kBlockSrc |+ " KBlock to the graph: " +|| kBlock ||+ "."
    let kBlock' = D.KBlockContent kBlock
    topKBlockHash <- L.readVar (bData ^. Lens.wTopKBlockHash)

    eMblocks <- getMBlocksForKBlock (bData ^. Lens.windowedGraph) topKBlockHash
    whenRight eMblocks $ \mBlocks -> forM_ mBlocks $ L.calculateLedger bData

    L.evalGraph (bData ^. Lens.wGraph) $ do
        L.newNode kBlock'
        L.newLink topKBlockHash kBlock'

    L.modifyVar (bData ^. Lens.wWindowSize) (+ 1)

    -- change of curNode.
    L.writeVar (bData ^. Lens.wTopKBlockHash) $ D.toHash kBlock'
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

shrinkGraphDownFrom :: D.WindowedGraph -> D.GraphNode -> L.StateL ()
shrinkGraphDownFrom wndGraph node = do
    preds <- getPredecessors wndGraph node
    case preds of
        ([], []) -> pure ()
        (nodes, rLinks) -> do
            L.logInfo $ "Deleting predecessor links, nodes: " <> show (length rLinks)
                <> foldr (\rLink s -> "\n    " <> show rLink <> s) "" rLinks
            L.evalGraph (D._graph wndGraph) $ do
                mapM_ (L.deleteLink (node ^. Lens.hash)) rLinks
                mapM_ (L.deleteNode . view Lens.hash) nodes
            mapM_ (shrinkGraphDownFrom wndGraph) nodes

shrinkGraph :: D.BlockchainData -> D.BlockNumber -> D.BlockNumber -> KNode -> L.StateL ()
shrinkGraph bData wndSizeThreshold bottomKBlockNumber kNode@(kBlock, _) = do
    let wndGraph = bData ^. Lens.windowedGraph
    mbKNode <- findKBlockNodeDownward' wndGraph (FindByNumber bottomKBlockNumber) (Just kNode)
    whenJust mbKNode $ \(_, node) -> do
        shrinkGraphDownFrom wndGraph node
        L.writeVar (wndGraph ^. Lens.bottomKBlockHash) $ node ^. Lens.hash
        L.writeVar (wndGraph ^. Lens.windowSize) wndSizeThreshold
