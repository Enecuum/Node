{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Framework.Lens where

import           Control.Lens             ( makeFieldsNoPrefix )

import           Enecuum.Framework.Domain
import           Enecuum.Framework.Node.Runtime

makeFieldsNoPrefix ''Connection
makeFieldsNoPrefix ''ConnectionConfig
makeFieldsNoPrefix ''NetworkRequest
makeFieldsNoPrefix ''NetworkResponse
makeFieldsNoPrefix ''NodeConfig

makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''NodeRuntime