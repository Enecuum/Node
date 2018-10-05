{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Types where

import Enecuum.Domain as D
import           Control.Lens                  (makeFieldsNoPrefix)

data NetworkNodeChainData = NetworkNodeChainData
  {
     _chainVar   :: D.StateVar [D.KBlock]
  }

makeFieldsNoPrefix ''NetworkNodeChainData