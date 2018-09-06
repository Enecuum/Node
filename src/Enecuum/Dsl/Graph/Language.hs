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


class StringHashable (Content a) => ToContent a b | a -> b, b -> a where
    toContent   :: b -> Content a
    fromContent :: Content a -> b


class ToRef a b | a -> b, b -> a where
    toRef   :: b -> Ref a
    fromRef :: Ref a -> b


newNode, deleteNode :: (ToContent config c, StringHashable c) => c -> Eff '[GraphDsl config] ()
newNode    = send . NewNode . toContent
deleteNode = send . DeleteNode . toContent


newLink, deleteLink :: ToContent config c => c -> c -> Eff '[GraphDsl config] ()
newLink a b = send (NewLink (toContent a) (toContent b))
deleteLink a b = send (DeleteLink (toContent a) (toContent b))


findNode :: StringHash -> Eff '[GraphDsl config] (Maybe (Content config, Set StringHash))
findNode = send . FindNode


deleteRNode :: Ref config -> Eff '[GraphDsl config] ()
deleteRNode = send . DeleteRNode

newRLink, deleteRLink :: Ref config -> Ref config -> Eff '[GraphDsl config] ()
newRLink    a b = send $ NewRLink a b
deleteRLink a b = send $ DeleteRLink a b

findRNode :: StringHash -> Eff '[GraphDsl config] (Maybe (Ref config, Map StringHash (Ref config)))
findRNode = send . FindRNode

data family Content a
data family Ref acosh

data GraphDsl config a where
    NewNode     :: Content config  -> GraphDsl config ()
    DeleteNode  :: Content config  -> GraphDsl config ()
    NewLink     :: Content config -> Content config -> GraphDsl config ()
    DeleteLink  :: Content config -> Content config -> GraphDsl config ()
    FindNode    :: StringHash -> GraphDsl config (Maybe (Content config, Set StringHash))

    DeleteRNode :: Ref config -> GraphDsl config ()
    NewRLink    :: Ref config -> Ref config -> GraphDsl config ()
    DeleteRLink :: Ref config -> Ref config -> GraphDsl config  ()
    FindRNode   :: StringHash -> GraphDsl config (Maybe (Ref config, Map StringHash (Ref config)))


