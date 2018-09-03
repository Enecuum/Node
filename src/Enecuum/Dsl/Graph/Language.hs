{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Enecuum.Dsl.Graph.Language where

import Universum
import Eff

class StringHashable a where
    toHash :: a -> ByteString


data GraphDsl a where
    NewNode     :: StringHashable content => content -> GraphDsl ()
    DeleteNode  :: StringHashable content => content -> GraphDsl ()
    NewLinck    :: ByteString -> ByteString -> GraphDsl ()
    DeleteLinck :: ByteString -> ByteString -> GraphDsl ()
    FindNode    :: ByteString -> GraphDsl (Maybe (content, Set ByteString))


newNode, deleteNode :: (Member GraphDsl r, StringHashable content) => content -> Eff r ()
newNode = send . NewNode
deleteNode = send . DeleteNode


newLinck, deleteLinck :: Member GraphDsl r => ByteString -> ByteString -> Eff r ()
newLinck a b = send (NewLinck a b)
deleteLinck a b = send (DeleteLinck a b)


findNode :: Member GraphDsl r => ByteString -> Eff r (Maybe (content, Set ByteString))
findNode = send . FindNode