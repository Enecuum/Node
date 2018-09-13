{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Enecuum.Core.HGraph.THGraph (
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
    ) where

import           Universum
import qualified Data.Map                      as M

import           Control.Concurrent.STM.TVar

import           Enecuum.Core.HGraph.StringHashable
import           Control.Lens (makeLenses)

-- | Graph is a set of the nodes.
type THGraph c = Map StringHash (TVar (THNode c))

-- | Node of graph is container for a content with links to and from tne node.
data THNode c = THNode
    { _links      :: THGraph c      -- | Links from the node
    , _rLinks     :: THGraph c      -- | Links to the node
    , _content    :: c              -- | Conten of the node
    }
makeLenses ''THNode

-- | Create a new graph.
newTHGraph :: StringHashable c => STM (TVar (THGraph c))
newTHGraph = newTVar mempty

-- | Add new node in the graph by content of the node.  
newNode :: StringHashable c => TVar (THGraph c) -> c -> STM Bool
newNode aIndex aContent = do
    let nodeHash = toHash aContent
    res <- findNode aIndex nodeHash
    when (isNothing res) $ do
        tHNode <- newTVar $ THNode mempty mempty aContent
        modifyTVar aIndex $ M.insert nodeHash tHNode
    return $ isNothing res

-- | Delete the node from the graph by hash of node content. 
deleteHNode :: StringHashable c => TVar (THGraph c) -> StringHash -> STM Bool
deleteHNode aIndex nodeHash = do
    tHNode <- findNode aIndex nodeHash
    whenJust tHNode $ deleteTHNode aIndex
    return $ isJust tHNode

-- | Creating/deleting of linck by hash of node contens. 
newHLink, deleteHLink :: StringHashable c => TVar (THGraph c) -> ReformLink StringHash
newHLink = reformHLink newTLink
deleteHLink = reformHLink deleteTLink

-- | Find node in graph by hash of node content.
findNode
    :: StringHashable c
    => TVar (THGraph c)
    -> StringHash
    -> STM (Maybe (TVar (THNode c)))
findNode aTHGraph aNodeName = M.lookup aNodeName <$> readTVar aTHGraph

-- | Delete the node from the graph by node ref. 
deleteTHNode :: StringHashable c => TVar (THGraph c) -> TVar (THNode c) -> STM ()
deleteTHNode aIndex tHNode = do
    aNode <- readTVar tHNode
    let nodeHash = toHash $ aNode ^. content
    modifyTVar aIndex $ M.delete nodeHash
    forM_ (aNode ^. rLinks)
        $ \aVar -> modifyTVar aVar (links %~ M.delete nodeHash)

-- | Creating/deleting of linck by node contens. 
newTLink, deleteTLink :: StringHashable c => ReformTLink c
newTLink n1 n2 = do
    node1 <- readTVar n1
    node2 <- readTVar n2
    let hasOfN2 = toHash (node2 ^. content)
        hasOfN1 = toHash (node1 ^. content)
        ok     = M.notMember hasOfN2 (node1 ^. links)
    when ok $ do
        modifyTVar n1 $ links %~ M.insert hasOfN2 n2
        modifyTVar n2 (rLinks %~ M.insert hasOfN1 n1)
    return ok

deleteTLink n1 n2 = do
    node1 <- readTVar n1
    node2 <- readTVar n2
    let hasOfN2 = toHash (node2 ^. content)
        hasOfN1 = toHash (node1 ^. content)
        ok     = M.member hasOfN2 (node1 ^. links)
    when ok $ do
        modifyTVar n1 $ links %~ M.delete hasOfN2
        modifyTVar n2 (rLinks %~ M.delete hasOfN1)
    return ok

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

type ReformLink  c = c -> c -> STM Bool
type ReformTLink c = ReformLink (TVar (THNode c))

reformHLink
    :: StringHashable c => ReformTLink c -> TVar (THGraph c) -> ReformLink StringHash
reformHLink f aIndex x1 x2 = do
    aNodes <- forM [x1, x2] (findNode aIndex)
    case catMaybes aNodes of
        [n1, n2] -> f n1 n2
        _        -> return False