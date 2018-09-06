{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Testing.Lens where

import Enecuum.Prelude (makeFieldsNoPrefix)

import Enecuum.Framework.Testing.Types
  ( NodeRpcServerControl
  , NodeRpcServerHandle
  , NodeRuntime
  , TestRuntime
  )

makeFieldsNoPrefix ''NodeRpcServerControl
makeFieldsNoPrefix ''NodeRpcServerHandle
makeFieldsNoPrefix ''NodeRuntime
makeFieldsNoPrefix ''TestRuntime
