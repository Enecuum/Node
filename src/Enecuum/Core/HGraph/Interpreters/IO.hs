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

module Enecuum.Core.HGraph.Interpreters.IO
    ( interpretHGraphLIO
    , runHGraphLIO
    , runHGraphIO
    ) where

import           Universum
import           Data.Serialize
import           Control.Monad.Free

import           Data.HGraph.THGraph as G
import qualified Enecuum.Core.HGraph.Internal.Impl as Impl

import           Data.HGraph.StringHashable (StringHashable)
import           Enecuum.Core.HGraph.Language (HGraphL (..))
import           Enecuum.Core.HGraph.Internal.Types (TNodeL)

-- | The interpreter of the language describing the action on graphs.
interpretHGraphLIO
    :: (Serialize c, StringHashable c)
    => TVar (G.THGraph c)
    -> HGraphL (TNodeL c) a
    -> IO a

-- create a new node
interpretHGraphLIO graph (NewNode x next) =
    next <$> (atomically $ Impl.newNode graph x)

-- get nodeby hash, content or ref
interpretHGraphLIO graph (GetNode x next) = do
    node <- atomically $ Impl.getNode graph x
    pure $ next node

-- delete node by hash, content or ref
interpretHGraphLIO graph (DeleteNode x next) = do
    ok <- atomically $ Impl.deleteNode graph x
    pure $ next ok

-- create new link by contents, hashes or refs of the node
interpretHGraphLIO graph (NewLink x y next) = do
    ok <- atomically $ Impl.newLink graph x y
    pure $ next ok

-- delete link inter a nodes by contents, hashes or refs of the node
interpretHGraphLIO graph (DeleteLink x y next) = do
    ok <- atomically $ Impl.deleteLink graph x y
    pure $ next ok

-- | Run H graph interpret.
runHGraphLIO, runHGraphIO
    :: (Serialize c, StringHashable c)
    => TVar (G.THGraph c)
    -> Free (HGraphL (TNodeL c)) w
    -> IO w
runHGraphLIO graph = foldFree (interpretHGraphLIO graph)

-- | Run H graph interpret in IO monad.
runHGraphIO  = runHGraphLIO
