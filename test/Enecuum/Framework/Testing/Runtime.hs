{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.Testing.Runtime where

import qualified Enecuum.Domain                as D

import Enecuum.RuntimeTmp          

-- data RuntimeSt = RuntimeSt
--   { _nodeTag :: D.NodeTag

--   }

-- makeLenses ''RuntimeSt

defaultRuntimeSt :: RuntimeSt
defaultRuntimeSt = RuntimeSt
