{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Lenses for Core Runtime types.
module Enecuum.Core.RLens where

import Enecuum.Core.Runtime     ( CoreRuntime )
import Control.Lens             ( makeFieldsNoPrefix )

makeFieldsNoPrefix ''CoreRuntime
