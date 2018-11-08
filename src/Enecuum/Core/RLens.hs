{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Lenses for Core Runtime types.
module Enecuum.Core.RLens where

import Enecuum.Core.Runtime     (CoreRuntime, LoggerRuntime, StateRuntime)
import Control.Lens             (makeFieldsNoPrefix)

makeFieldsNoPrefix ''LoggerRuntime
makeFieldsNoPrefix ''CoreRuntime
makeFieldsNoPrefix ''StateRuntime
