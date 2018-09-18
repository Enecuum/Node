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
    -- * Clases
    , HGraph(..)
    -- * Functions
    , newNode
    , newLink
    , deleteLink
    , deleteNode
    ) where

import           Universum
import           Control.Monad.Free
import           Enecuum.Core.HGraph.Types
    ( HNodeContent
    , HNodeRef
    , ToContent
    , ToNodeRef
    , toNodeRef
    , toContent
    )

data HGraphL node a where
    NewNode     :: HNodeContent node -> (Bool -> a) -> HGraphL node a
    DeleteNode  :: HNodeRef node -> (Bool -> a) -> HGraphL node a
    NewLink     :: HNodeRef node -> HNodeRef node -> (Bool -> a) -> HGraphL node a
    DeleteLink  :: HNodeRef node -> HNodeRef node -> (Bool -> a) -> HGraphL node a
    GetNode     :: HNodeRef node -> (Maybe node -> a) -> HGraphL node a
  deriving (Functor)

class Functor m => HGraph node m | m -> node where
    newLink', deleteLink'
        :: (ToNodeRef node b, ToNodeRef node c) => c -> b -> m Bool

    newNode'    :: ToContent node c => c -> m Bool
    deleteNode' :: ToNodeRef node h => h -> m Bool
    getNode     :: ToNodeRef node h => h -> m (Maybe node)

instance HGraph node (Free (HGraphL node)) where
    newLink' a b     = liftF (NewLink (toNodeRef a) (toNodeRef b) id)
    deleteLink' a b  = liftF (DeleteLink (toNodeRef a) (toNodeRef b) id)
    newNode' a       = liftF (NewNode (toContent a) id)
    deleteNode' a    = liftF (DeleteNode (toNodeRef a) id)
    getNode a        = liftF (GetNode (toNodeRef a) id)


newLink, deleteLink
    :: (HGraph node m, ToNodeRef node b, ToNodeRef node c) => c -> b -> m ()
newLink a b      = void $ newLink' a b
deleteLink a b   = void $ deleteLink' a b

deleteNode :: (HGraph node m, ToNodeRef node h) => h -> m ()
deleteNode = void . deleteNode'

newNode :: (HGraph node m, ToContent node c) => c -> m ()
newNode = void . newNode'