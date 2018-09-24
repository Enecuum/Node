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

module Enecuum.Core.HGraph.Internal.Impl where

import           Universum
import           Data.Serialize
import           Control.Monad.Free

import           Enecuum.Core.HGraph.Internal.Types
import           Data.HGraph.THGraph as G
import           Data.HGraph.StringHashable (StringHash, StringHashable, toHash)
import           Enecuum.Core.HGraph.Language ( HGraphL (..))
import           Enecuum.Core.HGraph.Types (HNodeRef, HNode (..), HNodeContent,
                                            ToNodeRef, ToContent,
                                            fromContent, toContent, toNodeRef)

-- | Init HGraph.
initHGraph :: (Serialize c, StringHashable c) => IO (TVar (G.THGraph c))
initHGraph = atomically G.newTHGraph

-- create a new node
newNode
  :: (StringHashable c, ToContent config c)
  => TVar (THGraph c)
  -> HNodeContent config
  -> STM Bool
newNode graph x = G.newNode graph (fromContent x)

-- get nodeby hash, content or ref
getNode
  :: StringHashable content
  => TVar (THGraph content)
  -> HNodeRef (HNode (TVar (THNode content)) content)
  -> STM (Maybe (HNode (TVar (THNode content)) content))
getNode graph x = do
    mbNode <- case x of
        TNodeRef tNode     -> return $ Just tNode
        TNodeHash nodeHash -> G.findNode graph nodeHash
    case mbNode of
        Nothing    -> return Nothing
        Just tNode -> do
            node <- readTVar tNode
            return $ Just $ HNode
                (toHash $ node ^. content)
                (TNodeRef tNode)
                (TNodeContent $ node ^. content)
                (TNodeRef <$> node ^. links)
                (TNodeRef <$> node ^. rLinks)

-- delete node by hash, content or ref
deleteNode
  :: StringHashable c
  => TVar (THGraph c)
  -> HNodeRef (HNode (TVar (THNode c)) c)
  -> STM Bool
deleteNode graph (TNodeHash hash) = G.deleteHNode graph hash
deleteNode graph (TNodeRef ref)   = G.deleteTHNode graph ref >> return True

-- create new link by contents, hashes or refs of the node
newLink
  :: StringHashable c
  => TVar (THGraph c)
  -> HNodeRef (HNode (TVar (THNode c)) c)
  -> HNodeRef (HNode (TVar (THNode c)) c)
  -> STM Bool
newLink graph x y = case (x, y) of
    (TNodeRef  r1, TNodeRef  r2) -> G.newTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> G.newHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> G.findNode graph r2 >>= \case
        Just tNode -> G.newTLink r1 tNode
        Nothing    -> return $ False
    (TNodeHash  r1, TNodeRef r2) -> G.findNode graph r1 >>= \case
        Just tNode -> G.newTLink tNode r2
        Nothing    -> return $ False

-- delete link inter a nodes by contents, hashes or refs of the node
deleteLink
  :: StringHashable c
  => TVar (THGraph c)
  -> HNodeRef (HNode (TVar (THNode c)) c)
  -> HNodeRef (HNode (TVar (THNode c)) c)
  -> STM Bool
deleteLink graph x y = case (x, y) of
    (TNodeRef  r1, TNodeRef  r2) -> G.deleteTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> G.deleteHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> G.findNode graph r2 >>= \case
        Just tNode -> G.deleteTLink r1 tNode
        Nothing    -> return $ False
    (TNodeHash  r1, TNodeRef r2) -> G.findNode graph r1 >>= \case
        Just tNode -> G.deleteTLink tNode r2
        Nothing    -> return $ False

clearGraph :: StringHashable c => TVar (THGraph c) -> STM ()
clearGraph = G.deleteGraph