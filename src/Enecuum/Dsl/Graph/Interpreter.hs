{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Dsl.Graph.Interpreter where

import           Universum
import           Data.Serialize
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal

import           Enecuum.Dsl.Graph.Language
import           Enecuum.TGraph as G
import           Enecuum.StringHashable


instance ToNodeRef (DslTNode content) (TVar (TNode content))  where
    toNodeRef   = TNodeRef


instance ToNodeRef (DslTNode content) (NodeRef (DslTNode content)) where
    toNodeRef = identity


instance ToNodeRef (DslTNode content) StringHash  where
    toNodeRef   = TNodeHash


instance StringHashable content => ToNodeRef (DslTNode content) content  where
    toNodeRef   = TNodeHash . toHash


instance Serialize c => Serialize (NodeContent (DslTNode c))


instance (Serialize c, StringHashable c) => StringHashable (NodeContent (DslTNode c)) where
    toHash (TNodeContent c) = toHash c


instance StringHashable c => ToContent (DslTNode c) c where
    toContent = TNodeContent
    fromContent (TNodeContent a) = a


type DslTNode content = DslNode (TVar (TNode content)) content


data instance NodeContent (DslNode (TVar (TNode content)) content)
    = TNodeContent content
  deriving (Generic)


data instance NodeRef     (DslNode (TVar (TNode content)) content)
    = TNodeRef (TVar (TNode content))
    | TNodeHash StringHash
  deriving (Generic)


initGraph :: StringHashable c => IO (TVar (TGraph c))
initGraph = atomically G.newTGraph


runGraph :: StringHashable c => TVar (TGraph c) -> Eff '[GraphDsl (DslTNode c)] w -> IO w
runGraph _ (Val x) = return x
runGraph aGraph (E u q) = case extract u of

    NewNode x   -> do
        aBool <- atomically $ G.newNode aGraph (fromContent x)
        runGraph aGraph (qApp q (W aBool))

    DeleteNode  x -> do
        aBool <- atomically $ case x of
            TNodeRef aRef -> do
                G.deleteTNode aGraph aRef
                return True
            TNodeHash aHash -> G.deleteHNode aGraph aHash

        runGraph aGraph (qApp q (W aBool))

    NewLink    x y -> do
        aBool <- atomically $ case (x, y) of
            (TNodeRef  r1, TNodeRef  r2) -> G.newTLink r1 r2
            (TNodeHash r1, TNodeHash r2) -> G.newHLink aGraph r1 r2
            (TNodeRef  r1, TNodeHash r2) -> do
                aMaybeNode <- G.findNode aGraph r2
                case aMaybeNode of
                    Just aTNode -> G.newTLink r1 aTNode
                    Nothing     -> return False
            (TNodeHash  r1, TNodeRef r2) -> do
                aMaybeNode <- G.findNode aGraph r1
                case aMaybeNode of
                    Just aTNode -> G.newTLink aTNode r2
                    Nothing     -> return False
        runGraph aGraph (qApp q (W aBool))

    DeleteLink x y -> do
        aBool <- atomically $ case (x, y) of
            (TNodeRef  r1, TNodeRef  r2) -> G.deleteTLink r1 r2
            (TNodeHash r1, TNodeHash r2) -> G.deleteHLink aGraph r1 r2
            (TNodeRef  r1, TNodeHash r2) -> do
                aMaybeNode <- G.findNode aGraph r2
                case aMaybeNode of
                    Just aTNode -> G.deleteTLink r1 aTNode
                    Nothing     -> return False
            (TNodeHash  r1, TNodeRef r2) -> do
                aMaybeNode <- G.findNode aGraph r1
                case aMaybeNode of
                    Just aTNode -> G.deleteTLink aTNode r2
                    Nothing     -> return False

        runGraph aGraph (qApp q (W aBool))
    

    GetNode    x   -> do
        aRes <- atomically $ do
            aMaybeNode <- case x of
                TNodeRef aVar   -> return $ Just aVar
                TNodeHash aHash -> G.findNode aGraph aHash
            case aMaybeNode of
                Just aTNode -> do
                    aNode <- readTVar aTNode
                    return $ Just $ Node 
                        (toHash $ aNode^.content)
                        (TNodeRef aTNode)
                        (TNodeContent $ aNode^.content)
                        (TNodeRef <$> aNode^.links) 
                Nothing -> return Nothing
                
        runGraph aGraph (qApp q aRes)

