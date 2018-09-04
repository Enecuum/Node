{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.Testing.Runtime where

import Enecuum.Prelude

import qualified Enecuum.Domain                as D

data RuntimeSt = RuntimeSt
  { _nodeTag :: D.NodeTag

  }

makeLenses ''RuntimeSt

defaultRuntimeSt :: RuntimeSt
defaultRuntimeSt = RuntimeSt ""
