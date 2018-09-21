module Enecuum.Assets.Nodes.MasterNode where

import           Enecuum.Prelude

import           Enecuum.Config (Config)
import qualified Enecuum.Language as L

masterNode :: Config -> L.NodeDefinitionL ()
masterNode _ = do
    L.logInfo "Master node starting..."
    L.logInfo "Master node finished."