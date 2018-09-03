{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Core.Lens where

import           Enecuum.Core.Types ( NetworkRequest, NetworkResponse )
import           Control.Lens       ( makeFieldsNoPrefix )


makeFieldsNoPrefix ''NetworkRequest
makeFieldsNoPrefix ''NetworkResponse