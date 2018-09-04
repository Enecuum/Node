{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Enecuum.Dsl.Graph.Interpreter where

import           Universum
import           GHC.Exts
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal

import           Enecuum.Dsl.Graph.Language
import           Enecuum.TGraph as G
import           Enecuum.StringHashable

initGraph :: StringHashable c => IO (TVar (TGraph c))
initGraph = atomically G.newTGraph

runGraph :: StringHashable c => TVar (TGraph c) -> Eff '[GraphDsl c] w -> IO w
runGraph _ (Val x) = return x
runGraph aGraph (E u q) = case extract u of
    NewNode     x   -> do
        void . atomically $ G.newNode aGraph x
        runGraph aGraph (qApp q ())

    DeleteNode  x   -> do
        void . atomically $ G.deleteNode aGraph x
        runGraph aGraph (qApp q ())

    NewLink    x y -> do
        void . atomically $ G.newLink aGraph x y
        runGraph aGraph (qApp q ())

    DeleteLink x y -> do
        void . atomically $ G.deleteLink aGraph x y
        runGraph aGraph (qApp q ())

    FindNode    x   -> do
        aRes <- atomically $ do
            aMaybeNode <- G.findNode aGraph x
            case aMaybeNode of
                Just aTNode -> do
                    aNode <- readTVar aTNode
                    return $ Just (aNode^.content, fromList $ keys $ aNode^.links) 
                Nothing -> return Nothing
                
        runGraph aGraph (qApp q aRes)