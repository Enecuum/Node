{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Enecuum.Core.HGraph.Lens where

import          Enecuum.Prelude

import          Enecuum.Core.HGraph.Types (HNode)

makeFieldsNoPrefix ''HNode
