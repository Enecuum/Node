{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Core.HGraph.Language (
    -- * Language
      HGraphL (..)
    , HGraphModel
    -- * Functions
    , newNode'
    , newNode
    , newLink'
    , newLink
    , deleteLink'
    , deleteLink
    , deleteNode'
    , deleteNode
    , getNode
    ) where

import           Universum
import           Eff
import           Eff.Exc
import           Eff.SafeIO

import           Enecuum.Core.HGraph.Types (HNodeContent, HNodeRef, W, ToContent, ToNodeRef,
                                            toNodeRef, toContent)

data HGraphL node a where
    NewNode     :: HNodeContent node -> HGraphL node (W node Bool)
    DeleteNode  :: HNodeRef node -> HGraphL node (W node Bool)
    NewLink     :: HNodeRef node -> HNodeRef node -> HGraphL node (W node Bool)
    DeleteLink  :: HNodeRef node -> HNodeRef node -> HGraphL node (W node Bool)
    GetNode     :: HNodeRef node -> HGraphL node (Maybe node)

type HGraphModel node = '[HGraphL node, SIO, Exc SomeException]

newLink', deleteLink'
    :: (ToNodeRef node b, ToNodeRef node c)
    => c -> b -> Eff (HGraphModel node) (W node Bool)
newLink' a b     = send $ NewLink (toNodeRef a) (toNodeRef b)
deleteLink' a b  = send $ DeleteLink (toNodeRef a) (toNodeRef b)

newLink, deleteLink
    :: (ToNodeRef node c, ToNodeRef node b)
    => c -> b -> Eff (HGraphModel node) ()
newLink a b = void $ newLink' a b
deleteLink a b = void $ deleteLink' a b

newNode' :: ToContent node c => c -> Eff (HGraphModel node) (W node Bool)
newNode' = send . NewNode . toContent

newNode :: ToContent node c => c -> Eff (HGraphModel node) ()
newNode = void . newNode'

deleteNode' :: ToNodeRef node h => h -> Eff (HGraphModel node) (W node Bool)
deleteNode' = send . DeleteNode . toNodeRef

deleteNode :: ToNodeRef node h => h -> Eff (HGraphModel node) ()
deleteNode = void . deleteNode'

getNode :: ToNodeRef node h => h -> Eff (HGraphModel node) (Maybe node)
getNode = send . GetNode . toNodeRef
