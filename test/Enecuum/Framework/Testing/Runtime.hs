{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.Testing.Runtime where

import Enecuum.Prelude

import qualified Enecuum.Domain                as D

data NodeRuntimeSt = NodeRuntimeSt
  { _nodeTag :: D.NodeTag
  , _nodeAddress :: Text

  }

makeLenses ''NodeRuntimeSt

defaultNodeRuntimeSt :: NodeRuntimeSt
defaultNodeRuntimeSt = NodeRuntimeSt "" ""


data TestRuntime = TestRuntime
  {

  }

mkTestRuntime :: IO TestRuntime
mkTestRuntime = pure TestRuntime