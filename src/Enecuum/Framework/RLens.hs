{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Lenses for Framework Runtime types.
module Enecuum.Framework.RLens where

import           Control.Lens             ( makeFieldsNoPrefix )

import           Enecuum.Framework.Node.Runtime

makeFieldsNoPrefix ''NodeRuntime
