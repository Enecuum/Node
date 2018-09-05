{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies#-}

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

newNode, deleteNode :: (ToContent config c, Member (GraphDsl config) r, StringHashable c) => c -> Eff r ()
newNode    = send . NewNode . toContent
deleteNode = send . DeleteNode . toContent


newLink, deleteLink :: (ToContent config c, Member (GraphDsl config) r, StringHashable c) => c -> c -> Eff r ()
newLink a b = send (NewLink (toContent a) (toContent b))
deleteLink a b = send (DeleteLink (toContent a) (toContent b))


findNode :: Member (GraphDsl content) r => StringHash -> Eff r (Maybe (Content content, Set StringHash))
findNode = send . FindNode

--
deleteRNode :: (ToRef config c, Member (GraphDsl config) r) => c -> Eff r ()
deleteRNode = send . DeleteRNode . toRef

newRLink, deleteRLink :: (ToRef config c, Member (GraphDsl config) r) => c -> c -> Eff r ()
newRLink a b = send (NewRLink (toRef a) (toRef b))
deleteRLink a b = send (DeleteRLink (toRef a) (toRef b))

findRNode :: Member (GraphDsl config) r => StringHash -> Eff r (Maybe (Ref config, Set StringHash))
findRNode = send . FindRNode

data family Content a
data family Ref a

data GraphDsl config a where
    NewNode     :: Content config  -> GraphDsl config ()
    DeleteNode  :: Content config  -> GraphDsl config ()
    NewLink     :: Content config  -> Content config  -> GraphDsl config ()
    DeleteLink  :: Content config  -> Content config  -> GraphDsl config ()
    FindNode    :: StringHash -> GraphDsl config (Maybe (Content config, Set StringHash))

    DeleteRNode :: Ref config -> GraphDsl config ()
    NewRLink    :: Ref config -> Ref config -> GraphDsl config ()
    DeleteRLink :: Ref config -> Ref config -> GraphDsl config  ()
    FindRNode   :: StringHash -> GraphDsl config (Maybe (Ref config, Set StringHash))





