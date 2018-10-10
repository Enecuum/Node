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

module Enecuum.Core.HGraph.Interpreters.STM
    ( interpretHGraphLSTM
    , runHGraphLSTM
    , runHGraphSTM
    ) where

import           Universum
import           Data.Serialize
import           Control.Monad.Free

import qualified Enecuum.Core.HGraph.Internal.Impl as Impl

import           Data.HGraph.StringHashable (StringHashable)
import           Enecuum.Core.HGraph.Language (HGraphF (..), HGraphL)
import           Enecuum.Core.HGraph.Internal.Types (TNodeL)
import           Enecuum.Core.HGraph.Types
-- | The interpreter of the language describing the action on graphs.
interpretHGraphLSTM :: (Serialize c, StringHashable c) => TGraph c -> HGraphF (TNodeL c) a -> STM a

-- create a new node
interpretHGraphLSTM graph (NewNode x next) = next <$> Impl.newNode graph x

-- get nodeby hash, content or ref
interpretHGraphLSTM graph (GetNode x next) = do
    node <- Impl.getNode graph x
    pure $ next node

-- delete node by hash, content or ref
interpretHGraphLSTM graph (DeleteNode x next) = do
    ok <- Impl.deleteNode graph x
    pure $ next ok

-- create new link by contents, hashes or refs of the node
interpretHGraphLSTM graph (NewLink x y next) = do
    ok <- Impl.newLink graph x y
    pure $ next ok

-- delete link inter a nodes by contents, hashes or refs of the node
interpretHGraphLSTM graph (DeleteLink x y next) = do
    ok <- Impl.deleteLink graph x y
    pure $ next ok

interpretHGraphLSTM graph (ClearGraph next) = do
    Impl.clearGraph graph
    pure $ next ()


-- | Run H graph interpret.
runHGraphLSTM, runHGraphSTM :: (Serialize c, StringHashable c) => TGraph c -> HGraphL c w -> STM w
runHGraphLSTM graph = foldFree (interpretHGraphLSTM graph)

-- | Run H graph interpret in STM monad.
runHGraphSTM = runHGraphLSTM
