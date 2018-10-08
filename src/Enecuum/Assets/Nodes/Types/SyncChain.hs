{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.Types.SyncChain where

import           Control.Lens    (makeFieldsNoPrefix)
import           Enecuum.Domain  as D
import           Enecuum.Prelude

data NetworkNodeChainData = NetworkNodeChainData
  {
     _chainVar :: D.StateVar [D.KBlock]
   , _logVar   :: D.StateVar [Text]
  }

makeFieldsNoPrefix ''NetworkNodeChainData
