{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeInType #-}

module Enecuum.Dsl.Graph.Language where

import Universum
import Control.Monad.Freer

import Enecuum.StringHashable
{-
data family Ref a
data family Content a
-}

data GraphDsl content a where
    NewNode     :: content  -> GraphDsl content ()
    DeleteNode  :: content  -> GraphDsl content ()
    NewLink     :: content  -> content  -> GraphDsl content ()
    DeleteLink  :: content  -> content  -> GraphDsl content ()
    FindNode    :: StringHash -> GraphDsl content (Maybe (content, Set StringHash))
{-
    DeleteRNode :: Ref content -> GraphDsl content ()
    NewRLink    :: Ref content -> Ref content -> GraphDsl content ()
    DeleteRLink :: Ref content -> Ref content -> GraphDsl content  ()
    FindRNode   :: StringHash -> GraphDsl content (Maybe (Ref content, Set StringHash))
-}
newNode, deleteNode :: (Member (GraphDsl content) r, StringHashable content) => content -> Eff r ()
newNode = send . NewNode
deleteNode = send . DeleteNode


newLink, deleteLink :: (Member (GraphDsl content) r, StringHashable content) => content -> content -> Eff r ()
newLink a b = send (NewLink a b)
deleteLink a b = send (DeleteLink a b)


findNode :: Member (GraphDsl content) r => StringHash -> Eff r (Maybe (content, Set StringHash))
findNode = send . FindNode
