{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Core.HGraph.Internal.Types where

import           Universum
import           Data.Serialize
import           Control.Monad.Free

import           Data.HGraph.THGraph (THNode)
import           Enecuum.Core.HGraph.Types (HNode)

import           Data.HGraph.THGraph as G
import           Data.HGraph.StringHashable (StringHash, StringHashable, toHash)
import           Enecuum.Core.HGraph.Language (HGraphL (..))
import           Enecuum.Core.HGraph.Types (HNodeRef, HNode (..), HNodeContent,
                                            ToNodeRef, ToContent,
                                            fromContent, toContent, toNodeRef)


-- This type doesn't look correct. It reveals implementation details
-- on the language level.

type TNodeL content = HNode (TVar (THNode content)) content

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

instance ToNodeRef (TNodeL content) (TVar (THNode content))  where
    toNodeRef = TNodeRef

instance ToNodeRef (TNodeL content) (HNodeRef (TNodeL content)) where
    toNodeRef = identity

instance ToNodeRef (TNodeL content) StringHash  where
    toNodeRef = TNodeHash

instance StringHashable content => ToNodeRef (TNodeL content) content  where
    toNodeRef = TNodeHash . toHash

instance Serialize c => Serialize (HNodeContent (TNodeL c))

instance (Serialize c, StringHashable c) => StringHashable (HNodeContent (TNodeL c)) where
    toHash (TNodeContent c) = toHash c

instance (Serialize c, StringHashable c) => ToContent (TNodeL c) c where
    toContent = TNodeContent
    fromContent (TNodeContent a) = a

data instance HNodeContent (HNode (TVar (THNode content)) content)
    = TNodeContent content
  deriving (Generic)

data instance HNodeRef (HNode (TVar (THNode content)) content)
    = TNodeRef (TVar (THNode content))
    | TNodeHash StringHash
  deriving (Generic)
