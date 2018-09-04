{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell#-}

module Enecuum.TGraph where

import           Universum
import qualified Data.Map                      as M
import qualified Data.List                     as L

import           Control.Concurrent.STM.TVar

import           Enecuum.StringHashable
import           Lens.Micro.TH

type TGraph c = Map ByteString (TVar (TNode c))

data TNode c = TNode {
    _links      :: TGraph c, 
    _rLinks     :: [TVar (TNode c)],
    _content    :: c
  }
makeLenses ''TNode


newTGraph :: StringHashable c => STM (TVar (TGraph c))
newTGraph = newTVar mempty

newNode, deleteNode :: StringHashable c => TVar (TGraph c) -> c -> STM Bool
newNode aIndex aContent = do
    let aNodeHash = toHash aContent
    aRes <- findNode aIndex aNodeHash
    when (isNothing aRes) $ do
        aTNode <- newTVar $ TNode mempty [] aContent
        modifyTVar aIndex $ M.insert aNodeHash aTNode
    return $ isNothing aRes

deleteNode aIndex aContent = do
    let aNodeHash = toHash aContent
    aMaybeTNode <- findNode aIndex aNodeHash
    whenJust aMaybeTNode $ \aTNode -> do
        aNode <- readTVar aTNode
        modifyTVar aIndex $ M.delete aNodeHash
        forM_ (aNode ^. rLinks)
            $ \aVar -> modifyTVar aVar (links %~ M.delete aNodeHash)
    return $ isJust aMaybeTNode


newLinck, deleteLinck :: StringHashable c => TVar (TGraph c) -> c -> c -> STM Bool
newLinck aIndex x1 x2 = do
    aNodes <- forM [x1, x2] (findNode aIndex . toHash)
    case catMaybes aNodes of
        [n1, n2] -> do
            aNode1 <- readTVar n1
            let hasOfN2 = toHash x2
                aOk     = M.notMember hasOfN2 (aNode1^.links)
            when aOk $ do
                modifyTVar n1 $ links %~ M.insert hasOfN2 n2
                modifyTVar n2 (rLinks %~ (n1 :))
            return aOk

        _        -> return False

deleteLinck aIndex x1 x2 = do
    aNodes <- forM [x1, x2] (findNode aIndex . toHash)
    case catMaybes aNodes of
        [n1, n2] -> do
            aNode1 <- readTVar n1
            let hasOfN2 = toHash x2
                aOk     = M.notMember hasOfN2 (aNode1^.links)
            when aOk $ do
                modifyTVar n1 $ links %~ M.delete hasOfN2
                modifyTVar n2 (rLinks %~ L.delete n1)
            return aOk

        _        -> return False

findNode :: StringHashable c => TVar (TGraph c) -> ByteString -> STM (Maybe (TVar (TNode c)))
findNode aTGraph aNodeName = M.lookup aNodeName <$> readTVar aTGraph
