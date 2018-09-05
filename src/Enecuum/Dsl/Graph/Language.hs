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


newNode, deleteNode :: (ToContent config c, Member (GraphDsl config) r, StringHashable c) => c -> Eff r ()
newNode    = send . NewNode . toContent
deleteNode = send . DeleteNode . toContent


newLink, deleteLink :: (ToContent config c, Member (GraphDsl config) r, StringHashable c) => c -> c -> Eff r ()
newLink a b = send (NewLink (toContent a) (toContent b))
deleteLink a b = send (DeleteLink (toContent a) (toContent b))


findNode :: Member (GraphDsl content) r => StringHash -> Eff r (Maybe (Content content, Set StringHash))
findNode = send . FindNode

data family Content a


data GraphDsl config a where
    NewNode     :: Content config  -> GraphDsl config ()
    DeleteNode  :: Content config  -> GraphDsl config ()
    NewLink     :: Content config  -> Content config  -> GraphDsl config ()
    DeleteLink  :: Content config  -> Content config  -> GraphDsl config ()
    FindNode    :: StringHash -> GraphDsl config (Maybe (Content config, Set StringHash))
{-
    DeleteRNode :: Ref config -> GraphDsl config ()
    NewRLink    :: Ref config -> Ref config -> GraphDsl config ()
    DeleteRLink :: Ref config -> Ref config -> GraphDsl config  ()
    FindRNode   :: StringHash -> GraphDsl config (Maybe (Ref config, Set StringHash))
-}





