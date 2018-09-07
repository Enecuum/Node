{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Core.Testing.Runtime.Lens where

import Enecuum.Prelude (makeFieldsNoPrefix)

import Enecuum.Core.Testing.Runtime.Types ( LoggerRuntime )

makeFieldsNoPrefix ''LoggerRuntime
