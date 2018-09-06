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


module Enecuum.Dsl.Graph.LangPoly where

import Universum
import Control.Monad.Freer

import Enecuum.StringHashable


class StringHashable (Content a) => ToContent a b | a -> b, b -> a where
    toContent   :: b -> Content a
    fromContent :: Content a -> b


class ToNodeRef a b | a -> b, b -> a where
    toNodeRef   :: b -> NodeRef a
    fromNodeRef :: NodeRef a -> b


data family Content a
data family NodeRef a


data DslNode ref content = Node {
    _nodeHash    :: StringHash,
    _nodeRef     :: NodeRef (DslNode ref content),
    _nodeContent :: Content (DslNode ref content),
    _nodeLinks   :: Map StringHash (NodeRef (DslNode ref content))
  }


data GraphDsl node a where
    NewNode     :: Content node -> GraphDsl node ()
    DeleteNode  :: NodeRef node -> GraphDsl node ()
    NewLink     :: NodeRef node -> NodeRef node -> GraphDsl node ()
    DeleteLink  :: NodeRef node -> NodeRef node -> GraphDsl node ()
    GetNode     :: NodeRef node -> GraphDsl node (Maybe node)