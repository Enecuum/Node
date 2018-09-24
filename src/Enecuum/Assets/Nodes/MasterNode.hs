module Enecuum.Assets.Nodes.MasterNode where

import           Enecuum.Prelude

import           Enecuum.Config (Config)
import qualified Enecuum.Language as L

masterNode :: Config -> L.NodeDefinitionL cfg ()
masterNode _ = do
    L.logInfo "Master node setting node tag..."
    L.nodeTag "master node"
    L.logInfo "Master node scenario evaluation..."
    L.scenario $ do
        L.logInfo "Variable test. Creating and reading Var."
        var <- L.atomically $ L.newVar @Int 10
        val <- L.atomically $ L.readVar var
        L.logInfo $ "Variable test: " +|| val ||+ " (should be 10)."
    L.logInfo "Master node definition finished."
