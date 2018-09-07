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

module Enecuum.Dsl.Graph.Language where

import Universum
import Control.Monad.Freer

import Enecuum.StringHashable


data GraphDsl node a where
    NewNode     :: NodeContent node -> GraphDsl node (W node Bool)
    DeleteNode  :: NodeRef node -> GraphDsl node (W node Bool)
    NewLink     :: NodeRef node -> NodeRef node -> GraphDsl node (W node Bool)
    DeleteLink  :: NodeRef node -> NodeRef node -> GraphDsl node (W node Bool)
    GetNode     :: NodeRef node -> GraphDsl node (Maybe node)


data W a b = W b


data family NodeContent a


data family NodeRef a


data DslNode ref content = Node {
    _nodeHash    :: StringHash,
    _nodeRef     :: NodeRef (DslNode ref content),
    _nodeContent :: NodeContent (DslNode ref content),
    _nodeLinks   :: Map StringHash (NodeRef (DslNode ref content))
  }


class StringHashable (NodeContent config) => ToContent config b | config -> b where
    toContent   :: b -> NodeContent config
    fromContent :: NodeContent config -> b


class ToNodeRef config b where
    toNodeRef   :: b -> NodeRef config


newLink', deleteLink' :: (ToNodeRef node b, ToNodeRef node c) => c -> b -> Eff '[GraphDsl node] (W node Bool)
newLink' a b     = send $ NewLink (toNodeRef a) (toNodeRef b)
deleteLink' a b  = send $ DeleteLink (toNodeRef a) (toNodeRef b)

newLink, deleteLink :: (ToNodeRef node c, ToNodeRef node b) => c -> b -> Eff '[GraphDsl node] ()
newLink a b    = void $ newLink' a b
deleteLink a b = void $ deleteLink' a b  

newNode' :: ToContent node c => c -> Eff '[GraphDsl node] (W node Bool)
newNode' = send . NewNode . toContent

newNode :: ToContent node c => c -> Eff '[GraphDsl node] ()
newNode = void . newNode'

deleteNode' :: ToNodeRef node h => h -> Eff '[GraphDsl node] (W node Bool)
deleteNode' = send . DeleteNode . toNodeRef

deleteNode :: ToNodeRef node h => h -> Eff '[GraphDsl node] ()
deleteNode = void . deleteNode'

getNode :: ToNodeRef node h => h -> Eff '[GraphDsl node] (Maybe node)
getNode = send . GetNode . toNodeRef
