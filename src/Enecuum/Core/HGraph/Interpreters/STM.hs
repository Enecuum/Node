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

module Enecuum.Core.HGraph.Interpreters.STM (
      initHGraph
    , interpretHGraphL
    , runHGraphL
    , runHGraph
    ) where

import           Universum
import           Data.Serialize
import           Control.Monad.Free

import           Data.HGraph.THGraph as G
import qualified Enecuum.Core.HGraph.Internal.Impl as Impl

import           Data.HGraph.StringHashable (StringHashable)
import           Enecuum.Core.HGraph.Language (HGraphL (..))
import           Enecuum.Core.HGraph.Internal.Types (TNodeL)

-- TODO: move this function to the appropriate place.
-- | Init HGraph.
initHGraph :: (Serialize c, StringHashable c) => IO (TVar (G.THGraph c))
initHGraph = atomically G.newTHGraph

-- | The interpreter of the language describing the action on graphs.
interpretHGraphL
    :: (Serialize c, StringHashable c)
    => TVar (G.THGraph c)
    -> HGraphL (TNodeL c) a
    -> STM a

-- create a new node
interpretHGraphL graph (NewNode x next) =
    next <$> Impl.newNode graph x

-- get nodeby hash, content or ref
interpretHGraphL graph (GetNode x next) = do
    node <- Impl.getNode graph x
    pure $ next node

-- delete node by hash, content or ref
interpretHGraphL graph (DeleteNode x next) = do
    ok <- Impl.deleteNode graph x
    pure $ next ok

-- create new link by contents, hashes or refs of the node
interpretHGraphL graph (NewLink x y next) = do
    ok <- Impl.newLink graph x y
    pure $ next ok

-- delete link inter a nodes by contents, hashes or refs of the node
interpretHGraphL graph (DeleteLink x y next) = do
    ok <- Impl.deleteLink graph x y
    pure $ next ok

-- | Run H graph interpret.
runHGraphL, runHGraph
    :: (Serialize c, StringHashable c)
    => TVar (G.THGraph c)
    -> Free (HGraphL (TNodeL c)) w
    -> STM w
runHGraphL graph = foldFree (interpretHGraphL graph)

-- | Run H graph interpret in STM monad.
runHGraph = runHGraphL
