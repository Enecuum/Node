{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Core.HGraph.Internal.Impl where

import           Universum
import           Data.Serialize

import           Enecuum.Core.HGraph.Internal.Types
import           Data.HGraph.THGraph as G
import           Data.HGraph.StringHashable (StringHashable, toHash)
import           Enecuum.Core.HGraph.Types (HNodeRef, HNode (..), HNodeContent,
                                            ToContent, fromContent, TGraph(..))

-- | Init HGraph.
initHGraph :: (Serialize c, StringHashable c) => IO (TGraph c)
initHGraph = TGraph <$> atomically G.newTHGraph

-- create a new node
newNode :: (StringHashable c, ToContent config c) => TGraph c -> HNodeContent config -> STM Bool
newNode (TGraph graph) x = G.newNode graph (fromContent x)

-- get nodeby hash, content or ref
getNode
    :: StringHashable content
    => TGraph content
    -> HNodeRef (HNode (TVar (THNode content)) content)
    -> STM (Maybe (HNode (TVar (THNode content)) content))
getNode (TGraph graph) x = do
    mbNode <- case x of
        TNodeRef  tNode    -> pure $ Just tNode
        TNodeHash nodeHash -> G.findNode graph nodeHash
    case mbNode of
        Nothing    -> pure Nothing
        Just tNode -> do
            node <- readTVar tNode
            pure $ Just $ HNode (toHash $ node ^. content)
                                  (TNodeRef tNode)
                                  (TNodeContent $ node ^. content)
                                  (TNodeRef <$> node ^. links)
                                  (TNodeRef <$> node ^. rLinks)

-- delete node by hash, content or ref
deleteNode :: StringHashable c => TGraph c -> HNodeRef (HNode (TVar (THNode c)) c) -> STM Bool
deleteNode (TGraph graph) (TNodeHash hash) = G.deleteHNode graph hash
deleteNode (TGraph graph) (TNodeRef  ref ) = G.deleteTHNode graph ref >> pure True

-- create new link by contents, hashes or refs of the node
newLink
    :: StringHashable c
    => TGraph c
    -> HNodeRef (HNode (TVar (THNode c)) c)
    -> HNodeRef (HNode (TVar (THNode c)) c)
    -> STM Bool
newLink (TGraph graph) x y = case (x, y) of
    (TNodeRef  r1, TNodeRef r2 ) -> G.newTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> G.newHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> G.findNode graph r2 >>= \case
        Just tNode -> G.newTLink r1 tNode
        Nothing    -> pure False
    (TNodeHash r1, TNodeRef r2) -> G.findNode graph r1 >>= \case
        Just tNode -> G.newTLink tNode r2
        Nothing    -> pure False

-- delete link inter a nodes by contents, hashes or refs of the node
deleteLink
    :: StringHashable c
    => TGraph c
    -> HNodeRef (HNode (TVar (THNode c)) c)
    -> HNodeRef (HNode (TVar (THNode c)) c)
    -> STM Bool
deleteLink (TGraph graph) x y = case (x, y) of
    (TNodeRef  r1, TNodeRef r2 ) -> G.deleteTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> G.deleteHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> G.findNode graph r2 >>= \case
        Just tNode -> G.deleteTLink r1 tNode
        Nothing    -> pure False
    (TNodeHash r1, TNodeRef r2) -> G.findNode graph r1 >>= \case
        Just tNode -> G.deleteTLink tNode r2
        Nothing    -> pure False

clearGraph :: StringHashable c => TGraph c -> STM ()
clearGraph (TGraph graph) = G.deleteGraph graph
