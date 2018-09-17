{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Core.HGraph.Internal.Types
    ( TNodeL
    ) where

import           Enecuum.Prelude

import           Data.HGraph.THGraph (THNode)
import           Enecuum.Core.HGraph.Types (HNode)

-- This type doesn't look correct. It reveals implementation details
-- on the language level.

type TNodeL content = HNode (TVar (THNode content)) content
