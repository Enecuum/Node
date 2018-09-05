{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Dsl.Graph.Interpreter where

import           Universum
import           GHC.Exts
import           Data.Serialize
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal

import           Enecuum.Dsl.Graph.Language
import           Enecuum.TGraph as G
import           Enecuum.StringHashable


data instance Content (TNode c) = TNodeContent c deriving (Generic)
data instance Ref (TNode c) = TNodeRef (TVar (TNode c)) deriving (Generic)

instance Serialize c => Serialize (Content (TNode c))
instance (Serialize c, StringHashable c) => StringHashable (Content (TNode c)) where
    toHash (TNodeContent c) = toHash c

instance StringHashable c => ToContent (TNode c) c where
    toContent = TNodeContent
    fromContent (TNodeContent a) = a


instance ToRef (TNode c) (TVar (TNode c)) where
    toRef = TNodeRef
    fromRef (TNodeRef a) = a


initGraph :: StringHashable c => IO (TVar (TGraph c))
initGraph = atomically G.newTGraph

runGraph :: StringHashable c => TVar (TGraph c) -> Eff '[GraphDsl (TNode c)] w -> IO w
runGraph _ (Val x) = return x
runGraph aGraph (E u q) = case extract u of
    NewNode x   -> do
        void . atomically $ G.newNode aGraph (fromContent x)
        runGraph aGraph (qApp q ())

    DeleteNode  x   -> do
        void . atomically $ G.deleteNode aGraph (fromContent x)
        runGraph aGraph (qApp q ())
    
    DeleteRNode x  -> do
        void . atomically $ G.deleteTNode aGraph (fromRef x)
        runGraph aGraph (qApp q ())

    NewLink    x y -> do
        void . atomically $ G.newLink aGraph (fromContent x) (fromContent y)
        runGraph aGraph (qApp q ())

    NewRLink    x y -> do
        void . atomically $ G.newTLink (fromRef x) (fromRef y)
        runGraph aGraph (qApp q ())

    DeleteLink x y -> do
        void . atomically $ G.deleteLink aGraph (fromContent x) (fromContent y)
        runGraph aGraph (qApp q ())
    
    DeleteRLink x y -> do
        void . atomically $ G.deleteTLink (fromRef x) (fromRef y)
        runGraph aGraph (qApp q ())

    FindNode    x   -> do
        aRes <- atomically $ do
            aMaybeNode <- G.findNode aGraph x
            case aMaybeNode of
                Just aTNode -> do
                    aNode <- readTVar aTNode
                    return $ Just (toContent $ aNode^.content, fromList $ keys $ aNode^.links) 
                Nothing -> return Nothing
                
        runGraph aGraph (qApp q aRes)

    FindRNode    x   -> do
        aRes <- atomically $ do
            aMaybeNode <- G.findNode aGraph x
            case aMaybeNode of
                Just aTNode -> do
                    aNode <- readTVar aTNode
                    return $ Just (toRef aTNode, fromList $ keys $ aNode^.links) 
                Nothing -> return Nothing
                
        runGraph aGraph (qApp q aRes)