{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE NoImplicitPrelude #-}

module Enecuum.TGraph where

import           Universum
import qualified Data.Map                      as M
import           Control.Concurrent.STM.TVar
import           Lens.Micro.TH


type TGraph a b = Map a (TVar (TNode a b))

data TNode a b = TNode {
    _tNodeName  :: a,
    _graphIndex :: TVar (TGraph a b),
    _links      :: Map a (TVar (TNode a b)),
    _rLinks     :: [TVar (TNode a b)],
    _content    :: b
  }

makeLenses ''TNode

newIndex :: Ord a => STM (TVar (TGraph a b))
newIndex = newTVar mempty


newTNode :: Ord a => TVar (TGraph a b) -> a -> b -> STM Bool
newTNode aIndex aName aContent = do
    aRes <- findNode aName aIndex
    when (isNothing aRes) $ do
        aTNode <- newTVar $ TNode aName aIndex mempty [] aContent
        modifyTVar aIndex $ M.insert aName aTNode
    return $ isNothing aRes


addTNode :: Ord a => TVar (TNode a b) -> a -> b -> STM Bool
addTNode aTNode aLinck aContent = do
    aNode <- readTVar aTNode
    aRes  <- findNode aLinck (aNode ^. graphIndex)
    when (isNothing aRes) $ do
        aNewTNode <- newTVar
            $ TNode aLinck (aNode ^. graphIndex) mempty [] aContent
        modifyTVar (aNode ^. graphIndex) $ M.insert aLinck aTNode
        modifyTVar aTNode    (links %~ M.insert aLinck aNewTNode)
        modifyTVar aNewTNode (rLinks %~ (aTNode :))
    return $ isNothing aRes


addLinck :: Ord a => TVar (TNode a b) -> TVar (TNode a b) -> STM ()
addLinck aTNode1 aTNode2 = do
    aNode2 <- readTVar aTNode2
    modifyTVar aTNode1 $ links %~ M.insert (aNode2 ^. tNodeName) aTNode2
    modifyTVar aTNode2 (rLinks %~ (aTNode1 :))


deleteLinck :: Ord a => a -> TVar (TNode a b) -> STM ()
deleteLinck aLinck aTNode = modifyTVar aTNode (links %~ M.delete aLinck)


deleteNode :: Ord a => TVar (TNode a b) -> STM ()
deleteNode aTNode = do
    aNode <- readTVar aTNode
    modifyTVar (aNode ^. graphIndex) $ M.delete (aNode ^. tNodeName)
    forM_ (aNode ^. rLinks)
        $ \aVar -> modifyTVar aVar (links %~ M.delete (aNode ^. tNodeName))


findNode :: Ord a => a -> TVar (TGraph a b) -> STM (Maybe (TVar (TNode a b)))
findNode aName aTIndex = M.lookup aName <$> readTVar aTIndex


mapGraph :: (b -> b) -> TVar (TGraph a b) -> STM ()
mapGraph f aTIndex = do
    aIndex <- readTVar aTIndex
    forM_ (elems aIndex) $ \aNode -> modifyTVar aNode (content %~ f)


foldGraph :: c -> (b -> c -> c) -> TVar (TGraph a b) -> STM c
foldGraph c f aTIndex = do
    aIndex <- readTVar aTIndex
    aLoop c (elems aIndex)
  where
    aLoop aCuum (x : xs) = do
        aNode <- readTVar x
        aLoop (f (aNode ^. content) aCuum) xs
    aLoop aCuum _ = return aCuum

