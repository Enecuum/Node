{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Core.HGraph.Types
    ( HNodeContent
    , HNodeRef
    , HNode (..)
    -- * Type Classes
    , ToContent (..)
    , ToNodeRef (..)
    , TGraph (..)
    , hashLinks
    ) where

import           Data.HGraph.StringHashable (StringHash, StringHashable)
import           Data.HGraph.THGraph        as G
import qualified Data.Map                   as Map
import           Enecuum.Prelude

data family HNodeContent a

data family HNodeRef a

data HNode ref content = HNode
    { _hash    :: StringHash
    , _ref     :: HNodeRef (HNode ref content)
    , _content :: HNodeContent (HNode ref content)
    , _links   :: Map StringHash (HNodeRef (HNode ref content))
    , _rLinks  :: Map StringHash (HNodeRef (HNode ref content))
    }

newtype TGraph content = TGraph (TVar (G.THGraph content))

class StringHashable (HNodeContent config) => ToContent config b | config -> b where
    toContent   :: b -> HNodeContent config
    fromContent :: HNodeContent config -> b

class ToNodeRef config b where
    toNodeRef   :: b -> HNodeRef config

hashLinks :: Map StringHash (HNodeRef (HNode ref content)) -> [StringHash]
hashLinks = Map.keys
