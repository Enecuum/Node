{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Enecuum.Dsl.Graph.Language where

import Universum
import Control.Monad.Freer

import Enecuum.StringHashable


data GraphDsl content a where
    NewNode     :: content -> GraphDsl content ()
    DeleteNode  :: content -> GraphDsl content ()
    NewLink    :: content -> content -> GraphDsl content ()
    DeleteLink :: content -> content -> GraphDsl content  ()
    FindNode    :: ByteString -> GraphDsl content (Maybe (content, Set ByteString))


newNode, deleteNode :: (Member (GraphDsl content) r, StringHashable content) => content -> Eff r ()
newNode = send . NewNode
deleteNode = send . DeleteNode


newLink, deleteLink :: (Member (GraphDsl content) r, StringHashable content) => content -> content -> Eff r ()
newLink a b = send (NewLink a b)
deleteLink a b = send (DeleteLink a b)


findNode :: Member (GraphDsl content) r => ByteString -> Eff r (Maybe (content, Set ByteString))
findNode = send . FindNode