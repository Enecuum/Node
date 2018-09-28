{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Testing.RLens where

import           Enecuum.Prelude (makeFieldsNoPrefix)

import           Enecuum.Testing.Types

makeFieldsNoPrefix ''LoggerRuntime
makeFieldsNoPrefix ''Control
makeFieldsNoPrefix ''RpcServerHandle
makeFieldsNoPrefix ''NodeRuntime
makeFieldsNoPrefix ''TestRuntime
