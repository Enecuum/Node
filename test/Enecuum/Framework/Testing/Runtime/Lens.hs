{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Testing.Runtime.Lens where

import Enecuum.Prelude (makeFieldsNoPrefix)

import Enecuum.Framework.Testing.Runtime.Types (NodeServerHandle, NodeRuntime)

makeFieldsNoPrefix ''NodeServerHandle
makeFieldsNoPrefix ''NodeRuntime
