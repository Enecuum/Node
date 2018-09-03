{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Framework.Lens where

import           Enecuum.Framework.Domain ( NodeConfig )
import           Control.Lens             ( makeFieldsNoPrefix )

makeFieldsNoPrefix ''NodeConfig