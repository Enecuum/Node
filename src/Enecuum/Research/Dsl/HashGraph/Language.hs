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

module Enecuum.Research.Dsl.HashGraph.Language where

import Universum
import Eff

import Enecuum.Research.StringHashable


data HashGraphDsl node a where
    NewNode     :: HashNodeContent node -> HashGraphDsl node (W node Bool)
    DeleteNode  :: HashNodeRef node -> HashGraphDsl node (W node Bool)
    NewLink     :: HashNodeRef node -> HashNodeRef node -> HashGraphDsl node (W node Bool)
    DeleteLink  :: HashNodeRef node -> HashNodeRef node -> HashGraphDsl node (W node Bool)
    GetNode     :: HashNodeRef node -> HashGraphDsl node (Maybe node)


newtype W a b = W b


data family HashNodeContent a


data family HashNodeRef a


data DslHashNode ref content = DslHashNode {
    _nodeHash    :: StringHash,
    _nodeRef     :: HashNodeRef (DslHashNode ref content),
    _nodeContent :: HashNodeContent (DslHashNode ref content),
    _nodeLinks   :: Map StringHash (HashNodeRef (DslHashNode ref content)),
    _noderLinks  :: Map StringHash (HashNodeRef (DslHashNode ref content))
  }


class StringHashable (HashNodeContent config) => ToContent config b | config -> b where
    toContent   :: b -> HashNodeContent config
    fromContent :: HashNodeContent config -> b


class ToNodeRef config b where
    toNodeRef   :: b -> HashNodeRef config


newLink', deleteLink' :: (ToNodeRef node b, ToNodeRef node c) => c -> b -> Eff '[HashGraphDsl node] (W node Bool)
newLink' a b     = send $ NewLink (toNodeRef a) (toNodeRef b)
deleteLink' a b  = send $ DeleteLink (toNodeRef a) (toNodeRef b)

newLink, deleteLink :: (ToNodeRef node c, ToNodeRef node b) => c -> b -> Eff '[HashGraphDsl node] ()
newLink a b    = void $ newLink' a b
deleteLink a b = void $ deleteLink' a b

newNode' :: ToContent node c => c -> Eff '[HashGraphDsl node] (W node Bool)
newNode' = send . NewNode . toContent

newNode :: ToContent node c => c -> Eff '[HashGraphDsl node] ()
newNode = void . newNode'

deleteNode' :: ToNodeRef node h => h -> Eff '[HashGraphDsl node] (W node Bool)
deleteNode' = send . DeleteNode . toNodeRef

deleteNode :: ToNodeRef node h => h -> Eff '[HashGraphDsl node] ()
deleteNode = void . deleteNode'

getNode :: ToNodeRef node h => h -> Eff '[HashGraphDsl node] (Maybe node)
getNode = send . GetNode . toNodeRef
