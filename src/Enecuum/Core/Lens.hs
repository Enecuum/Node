{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Lenses for Core types.
module Enecuum.Core.Lens where

import           Control.Lens              ( makeFieldsNoPrefix, makeLenses )

import           Enecuum.Core.HGraph.Types (HNode)
import           Enecuum.Core.Types.Logger (LoggerConfig)

makeLenses         ''HNode
makeFieldsNoPrefix ''LoggerConfig
