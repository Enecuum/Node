{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Testing.Lens where

import Enecuum.Prelude (makeFieldsNoPrefix)

import Enecuum.Framework.Testing.Types
  ( NetworkControl
  , RpcServerControl
  , RpcServerHandle
  , NodeRuntime
  , TestRuntime
  )

makeFieldsNoPrefix ''NetworkControl
makeFieldsNoPrefix ''RpcServerControl
makeFieldsNoPrefix ''RpcServerHandle
makeFieldsNoPrefix ''NodeRuntime
makeFieldsNoPrefix ''TestRuntime
