{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Enecuum.Core.HGraph.THGraph where

import           Universum
import qualified Data.Map                      as M

import           Control.Concurrent.STM.TVar

import           Enecuum.Core.HGraph.StringHashable
import           Control.Lens (makeLenses)

type THGraph c = Map StringHash (TVar (THNode c))

data THNode c = THNode
    { _links      :: THGraph c
    , _rLinks     :: THGraph c
    , _content    :: c
    }

makeLenses ''THNode

newTHGraph :: StringHashable c => STM (TVar (THGraph c))
newTHGraph = newTVar mempty

newNode, deleteNode :: StringHashable c => TVar (THGraph c) -> c -> STM Bool
newNode aIndex aContent = do
    let nodeHash = toHash aContent
    res <- findNode aIndex nodeHash
    when (isNothing res) $ do
        tHNode <- newTVar $ THNode mempty mempty aContent
        modifyTVar aIndex $ M.insert nodeHash tHNode
    return $ isNothing res

deleteNode aIndex = deleteHNode aIndex . toHash

deleteHNode :: StringHashable c => TVar (THGraph c) -> StringHash -> STM Bool
deleteHNode aIndex nodeHash = do
    tHNode <- findNode aIndex nodeHash
    whenJust tHNode $ deleteTHNode aIndex
    return $ isJust tHNode

newLink, deleteLink :: StringHashable c => TVar (THGraph c) -> ReformLink c
newLink = reformLink newTLink
deleteLink = reformLink deleteTLink

newHLink, deleteHLink :: StringHashable c => TVar (THGraph c) -> ReformLink StringHash
newHLink = reformHLink newTLink
deleteHLink = reformHLink deleteTLink

reformLink
    :: StringHashable c => ReformTLink c -> TVar (THGraph c) -> ReformLink c
reformLink f aIndex x1 x2 = reformHLink f aIndex (toHash x1) (toHash x2)

reformHLink
    :: StringHashable c => ReformTLink c -> TVar (THGraph c) -> ReformLink StringHash
reformHLink f aIndex x1 x2 = do
    aNodes <- forM [x1, x2] (findNode aIndex)
    case catMaybes aNodes of
        [n1, n2] -> f n1 n2
        _        -> return False

findNode
    :: StringHashable c
    => TVar (THGraph c)
    -> StringHash
    -> STM (Maybe (TVar (THNode c)))
findNode aTHGraph aNodeName = M.lookup aNodeName <$> readTVar aTHGraph

deleteTHNode :: StringHashable c => TVar (THGraph c) -> TVar (THNode c) -> STM ()
deleteTHNode aIndex tHNode = do
    aNode <- readTVar tHNode
    let nodeHash = toHash $ aNode ^. content
    modifyTVar aIndex $ M.delete nodeHash
    forM_ (aNode ^. rLinks)
        $ \aVar -> modifyTVar aVar (links %~ M.delete nodeHash)

type ReformLink  c = c -> c -> STM Bool
type ReformTLink c = ReformLink (TVar (THNode c))

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
