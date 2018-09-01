{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.Testing.Runtime where

import           Lens.Micro.TH                       ( makeLenses )

import qualified Enecuum.Domain                as D

data RuntimeSt = RuntimeSt
  { _nodeTag :: D.NodeTag

  }

makeLenses ''RuntimeSt

defaultRuntimeSt :: RuntimeSt
defaultRuntimeSt = RuntimeSt ""
