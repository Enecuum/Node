module Enecuum.Assets.Nodes.PoW where

import qualified Enecuum.Language              as L
import           Enecuum.Prelude

-- powNode :: L.NodeDefinitionL ()
powNode = do
  L.nodeTag "PoW"
  L.logInfo "Generate Key Block"
  -- L.getRandomInt (5,10)
  -- replicateM 10 $ L.getRandomInt (5,10)
