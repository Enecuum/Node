{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Core.HGraph.Types
    ( W (..)
    , HNodeContent
    , HNodeRef
    , HNode (..)
    -- * Clases
    , ToContent (..)
    , ToNodeRef (..)
    ) where

import           Enecuum.Prelude

import           Data.HGraph.StringHashable (StringHashable, StringHash)
import           Data.HGraph.THGraph (THNode)

newtype W a b = W b

data family HNodeContent a

data family HNodeRef a

data HNode ref content = HNode
    { _hash    :: StringHash
    , _ref     :: HNodeRef (HNode ref content)
    , _content :: HNodeContent (HNode ref content)
    , _links   :: Map StringHash (HNodeRef (HNode ref content))
    , _rLinks  :: Map StringHash (HNodeRef (HNode ref content))
    }

class StringHashable (HNodeContent config) => ToContent config b | config -> b where
    toContent   :: b -> HNodeContent config
    fromContent :: HNodeContent config -> b

class ToNodeRef config b where
    toNodeRef   :: b -> HNodeRef config
