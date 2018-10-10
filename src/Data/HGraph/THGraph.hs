{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.HGraph.THGraph (
    -- * Data
      THGraph
    , THNode
    -- * Lenses
    , rLinks
    , links
    , content
    -- * Work with the graph
    , newNode
    , newTHGraph
    , deleteTHNode
    , deleteHNode
    , deleteTLink
    , deleteHLink
    , newTLink
    , newHLink
    , findNode
    , deleteGraph
    ) where

import           Universum
import qualified Data.Map                      as M

import           Control.Concurrent.STM.TVar

import           Data.HGraph.StringHashable
import           Control.Lens (makeLenses)
import           Control.Lens.Setter ((.=))

-- | Graph is a set of the nodes.
type THGraph c = Map StringHash (TVar (THNode c))

-- | Node of graph is container for a content with links to and from tne node.
data THNode c = THNode
    { _links      :: THGraph c      -- ^ Links from the node
    , _rLinks     :: THGraph c      -- ^ Links to the node
    , _content    :: c              -- ^ Content of the node
    }
makeLenses ''THNode

-- | Create a new graph.
newTHGraph :: StringHashable c => STM (TVar (THGraph c))
newTHGraph = newTVar mempty

-- | Add new node to the graph by content of the node.
newNode :: StringHashable c => TVar (THGraph c) -> c -> STM Bool
newNode index nodeContent = do
    let nodeHash = toHash nodeContent
    res <- findNode index nodeHash
    when (isNothing res) $ do
        tNode <- newTVar $ THNode mempty mempty nodeContent
        modifyTVar index $ M.insert nodeHash tNode
    pure $ isNothing res

-- | Delete the node from the graph by hash of node content.
deleteHNode :: StringHashable c => TVar (THGraph c) -> StringHash -> STM Bool
deleteHNode index nodeHash = do
    tNode <- findNode index nodeHash
    whenJust tNode $ deleteTHNode index
    pure $ isJust tNode

-- | Creating/deleting of link by hash of node contens.
newHLink, deleteHLink :: StringHashable c => TVar (THGraph c) -> ReformLink StringHash
newHLink = reformHLink newTLink
deleteHLink = reformHLink deleteTLink

-- | Find node in graph by hash of node content.
findNode :: StringHashable c => TVar (THGraph c) -> StringHash -> STM (Maybe (TVar (THNode c)))
findNode graph nodeName = M.lookup nodeName <$> readTVar graph

-- | Delete the node from the graph by node ref.
deleteTHNode :: StringHashable c => TVar (THGraph c) -> TVar (THNode c) -> STM ()
deleteTHNode index tNode = do
    node <- readTVar tNode
    let nodeHash = toHash $ node ^. content
    modifyTVar index $ M.delete nodeHash
    forM_ (node ^. rLinks) $ \aVar -> modifyTVar aVar (links %~ M.delete nodeHash)

-- | Delete all nodes of graph.
deleteGraph :: StringHashable c => TVar (THGraph c) -> STM ()
deleteGraph index = do
    graph <- readTVar index
    writeTVar index mempty
    forM_ (elems graph) $ \var -> modifyTVar var $ execState $ do
        links .= mempty
        rLinks .= mempty

-- | Creating/deleting of link by node contens.
newTLink, deleteTLink :: StringHashable c => ReformTLink c
newTLink n1 n2 = do
    node1 <- readTVar n1
    node2 <- readTVar n2
    let hasOfN2 = toHash (node2 ^. content)
        hasOfN1 = toHash (node1 ^. content)
        ok      = M.notMember hasOfN2 (node1 ^. links)
    when ok $ do
        modifyTVar n1 $ links %~ M.insert hasOfN2 n2
        modifyTVar n2 (rLinks %~ M.insert hasOfN1 n1)
    pure ok

deleteTLink n1 n2 = do
    node1 <- readTVar n1
    node2 <- readTVar n2
    let hasOfN2 = toHash (node2 ^. content)
        hasOfN1 = toHash (node1 ^. content)
        ok      = M.member hasOfN2 (node1 ^. links)
    when ok $ do
        modifyTVar n1 $ links %~ M.delete hasOfN2
        modifyTVar n2 (rLinks %~ M.delete hasOfN1)
    pure ok

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

type ReformLink  c = c -> c -> STM Bool
type ReformTLink c = ReformLink (TVar (THNode c))

reformHLink :: StringHashable c => ReformTLink c -> TVar (THGraph c) -> ReformLink StringHash
reformHLink f index x1 x2 = do
    aNodes <- forM [x1, x2] (findNode index)
    case catMaybes aNodes of
        [n1, n2] -> f n1 n2
        _        -> pure False
