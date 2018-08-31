{-# LANGUAGE TemplateHaskell #-}

module Enecuum.TGraph where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Data.Map
import           Lens.Micro
import           Lens.Micro.TH


-- o. graph
data TNode a b = TNode {
    _links   :: Map a (TVar (TNode a b)),
    _rLinks  :: [TVar (TNode a b)],
    _content :: b
  }

makeLenses ''TNode

newTNode :: Ord a => b -> STM (TVar (TNode a b))
newTNode = newTVar . TNode mempty []


addTNode :: Ord a => TVar (TNode a b) -> a -> b -> STM ()
addTNode aTNode aLink aContent = do
    aNode1 <- readTVar aTNode
    when (notMember aLink $ aNode1^.links) $ do
        aNewTNode <- newTNode aContent
        modifyTVar aTNode (links %~ insert aLink aNewTNode)
        modifyTVar aNewTNode (rLinks %~ (aTNode:))


addLinck :: Ord a => a -> TVar (TNode a b) -> TVar (TNode a b) -> STM ()
addLinck aLink aTNode1 aTNode2 = do
    aNode1 <- readTVar aTNode1
    when (notMember aLink $ aNode1^.links) $ do
        modifyTVar aTNode1 (links %~ insert aLink aTNode2)
        modifyTVar aTNode2 (rLinks %~ (aTNode1:))


deleteLinck :: Ord a => a -> TVar (TNode a b) -> STM ()
deleteLinck = undefined


deleteNode :: Ord a => TVar (TNode a b) -> STM ()
deleteNode = undefined
