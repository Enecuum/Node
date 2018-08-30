{-# LANGUAGE TemplateHaskell#-}

module Enecuum.TGraph where

import Data.Map
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Lens.Micro
import Lens.Micro.TH


-- o. graph
data TNode a b = TNode {
    _lincks  :: Map a (TVar (TNode a b)),
    _rLincks :: [TVar (TNode a b)],
    _content :: b
  }

makeLenses ''TNode

newTNode :: Ord a => b -> STM (TVar (TNode a b))
newTNode = newTVar . TNode mempty []


addTNode :: Ord a => TVar (TNode a b) -> a -> b -> STM ()
addTNode aTNode aLinck aContent = do
    aNode1 <- readTVar aTNode
    when (notMember aLinck $ aNode1^.lincks) $ do
        aNewTNode <- newTNode aContent
        modifyTVar aTNode (lincks %~ insert aLinck aNewTNode)
        modifyTVar aNewTNode (rLincks %~ (aTNode:))


addLinck :: Ord a => a -> TVar (TNode a b) -> TVar (TNode a b) -> STM ()
addLinck aLinck aTNode1 aTNode2 = do
    aNode1 <- readTVar aTNode1
    when (notMember aLinck $ aNode1^.lincks) $ do
        modifyTVar aTNode1 (lincks %~ insert aLinck aTNode2)
        modifyTVar aTNode2 (rLincks %~ (aTNode1:))


deleteLinck :: Ord a => a -> TVar (TNode a b) -> STM ()
deleteLinck = undefined


deleteNode :: Ord a => TVar (TNode a b) -> STM ()
deleteNode = undefined