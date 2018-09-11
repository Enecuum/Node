{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Testing.Lens where

import Enecuum.Prelude (makeFieldsNoPrefix)

import Enecuum.Framework.Testing.Types
  ( Control
  , RpcServerHandle
  , NodeRuntime
  , TestRuntime
  )

makeFieldsNoPrefix ''Control
makeFieldsNoPrefix ''RpcServerHandle
makeFieldsNoPrefix ''NodeRuntime
makeFieldsNoPrefix ''TestRuntime
