{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Testing.Runtime.Lens where

import Enecuum.Prelude (makeFieldsNoPrefix)

import Enecuum.Framework.Testing.Runtime.Types
  ( NodeRpcServerControl
  , NodeRpcServerHandle
  , NodeRuntime
  , TestRuntime
  )

makeFieldsNoPrefix ''NodeRpcServerControl
makeFieldsNoPrefix ''NodeRpcServerHandle
makeFieldsNoPrefix ''NodeRuntime
makeFieldsNoPrefix ''TestRuntime
