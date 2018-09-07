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

module Enecuum.Research.Dsl.HashGraph.Interpreter where

import           Universum
import           Data.Serialize
import           Eff
import           Eff.Internal

import           Enecuum.Research.Dsl.HashGraph.Language
import           Enecuum.Research.THashGraph as G
import           Enecuum.Research.StringHashable


instance ToNodeRef (DslTNode content) (TVar (THashNode content))  where
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


type DslTNode content = DslHashNode (TVar (THashNode content)) content


data instance NodeContent (DslHashNode (TVar (THashNode content)) content)
    = TNodeContent content
  deriving (Generic)


data instance NodeRef     (DslHashNode (TVar (THashNode content)) content)
    = TNodeRef (TVar (THashNode content))
    | TNodeHash StringHash
  deriving (Generic)


initHashGraph :: StringHashable c => IO (TVar (THashGraph c))
initHashGraph = atomically G.newTHashGraph


runHashGraph :: StringHashable c => TVar (THashGraph c) -> Eff '[HashGraphDsl (DslTNode c)] w -> IO w
runHashGraph _ (Val x) = return x
runHashGraph aGraph (E u q) = case extract u of

    NewNode x   -> do
        aBool <- atomically $ G.newNode aGraph (fromContent x)
        runHashGraph aGraph (qApp q (W aBool))

    DeleteNode  x -> do
        aBool <- atomically $ case x of
            TNodeRef aRef -> do
                G.deleteTHashNode aGraph aRef
                return True
            TNodeHash aHash -> G.deleteHNode aGraph aHash

        runHashGraph aGraph (qApp q (W aBool))

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
        runHashGraph aGraph (qApp q (W aBool))

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

        runHashGraph aGraph (qApp q (W aBool))
    

    GetNode    x   -> do
        aRes <- atomically $ do
            aMaybeNode <- case x of
                TNodeRef aVar   -> return $ Just aVar
                TNodeHash aHash -> G.findNode aGraph aHash
            case aMaybeNode of
                Just aTNode -> do
                    aNode <- readTVar aTNode
                    return $ Just $ DslHashNode 
                        (toHash $ aNode^.content)
                        (TNodeRef aTNode)
                        (TNodeContent $ aNode^.content)
                        (TNodeRef <$> aNode^.links)
                        (TNodeRef <$> aNode^.rLinks) 
                Nothing -> return Nothing
                
        runHashGraph aGraph (qApp q aRes)

