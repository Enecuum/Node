{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Enecuum.Research.THashGraph where

import           Universum
import qualified Data.Map                      as M

import           Control.Concurrent.STM.TVar

import           Enecuum.Research.StringHashable
import           Control.Lens (makeLenses)

type THashGraph c = Map StringHash (TVar (THashNode c))

data THashNode c = THashNode {
    _links      :: THashGraph c,
    _rLinks     :: THashGraph c,
    _content    :: c
  }
makeLenses ''THashNode


newTHashGraph :: StringHashable c => STM (TVar (THashGraph c))
newTHashGraph = newTVar mempty

newNode, deleteNode :: StringHashable c => TVar (THashGraph c) -> c -> STM Bool
newNode aIndex aContent = do
    let aNodeHash = toHash aContent
    aRes <- findNode aIndex aNodeHash
    when (isNothing aRes) $ do
        aTHashNode <- newTVar $ THashNode mempty mempty aContent
        modifyTVar aIndex $ M.insert aNodeHash aTHashNode
    return $ isNothing aRes

deleteNode aIndex = deleteHNode aIndex . toHash


deleteHNode :: StringHashable c => TVar (THashGraph c) -> StringHash -> STM Bool
deleteHNode aIndex aNodeHash = do
    aTHashNode <- findNode aIndex aNodeHash
    whenJust aTHashNode $ deleteTHashNode aIndex
    return $ isJust aTHashNode


newLink, deleteLink :: StringHashable c => TVar (THashGraph c) -> ReformLink c
newLink = reformLink newTLink
deleteLink = reformLink deleteTLink


newHLink, deleteHLink :: StringHashable c => TVar (THashGraph c) -> ReformLink StringHash
newHLink = reformHLink newTLink
deleteHLink = reformHLink deleteTLink

reformLink
    :: StringHashable c => ReformTLink c -> TVar (THashGraph c) -> ReformLink c
reformLink f aIndex x1 x2 = reformHLink f aIndex (toHash x1) (toHash x2)


reformHLink
    :: StringHashable c => ReformTLink c -> TVar (THashGraph c) -> ReformLink StringHash
reformHLink f aIndex x1 x2 = do
    aNodes <- forM [x1, x2] (findNode aIndex)
    case catMaybes aNodes of
        [n1, n2] -> f n1 n2
        _        -> return False

findNode
    :: StringHashable c
    => TVar (THashGraph c)
    -> StringHash
    -> STM (Maybe (TVar (THashNode c)))
findNode aTHashGraph aNodeName = M.lookup aNodeName <$> readTVar aTHashGraph


deleteTHashNode :: StringHashable c => TVar (THashGraph c) -> TVar (THashNode c) -> STM ()
deleteTHashNode aIndex aTHashNode = do
    aNode <- readTVar aTHashNode
    let aNodeHash = toHash $ aNode ^. content
    modifyTVar aIndex $ M.delete aNodeHash
    forM_ (aNode ^. rLinks)
        $ \aVar -> modifyTVar aVar (links %~ M.delete aNodeHash)

type ReformLink  c = c -> c -> STM Bool
type ReformTLink c = ReformLink (TVar (THashNode c))

newTLink, deleteTLink :: StringHashable c => ReformTLink c
newTLink n1 n2 = do
    aNode1 <- readTVar n1
    aNode2 <- readTVar n2
    let hasOfN2 = toHash (aNode2 ^. content)
        hasOfN1 = toHash (aNode1 ^. content)
        aOk     = M.notMember hasOfN2 (aNode1 ^. links)
    when aOk $ do
        modifyTVar n1 $ links %~ M.insert hasOfN2 n2
        modifyTVar n2 (rLinks %~ M.insert hasOfN1 n1)
    return aOk

deleteTLink n1 n2 = do
    aNode1 <- readTVar n1
    aNode2 <- readTVar n2
    let hasOfN2 = toHash (aNode2 ^. content)
        hasOfN1 = toHash (aNode1 ^. content)
        aOk     = M.member hasOfN2 (aNode1 ^. links)
    when aOk $ do
        modifyTVar n1 $ links %~ M.delete hasOfN2
        modifyTVar n2 (rLinks %~ M.delete hasOfN1)
    return aOk
