{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.TestData.Helpers where

import Enecuum.Prelude

import qualified Enecuum.Language                     as L
import qualified Enecuum.Framework.TestData.TestGraph as TG
import           Enecuum.Framework.Environment

class HasGraph s a | s -> a where
  graph :: Lens' s a


withGraph
  :: HasGraph s TG.TestGraphVar
  => s
  -> TG.TestGraphL a
  -> L.StateL a
withGraph s = L.evalGraph (s ^. graph)

withGraphIO
  :: HasGraph s TG.TestGraphVar
  => s
  -> TG.TestGraphL a
  -> L.NodeL a
withGraphIO s = L.evalGraphIO (s ^. graph)
