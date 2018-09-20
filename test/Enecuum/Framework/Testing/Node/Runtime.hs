-- | This module contains functions to work with node runtime.
module Enecuum.Framework.Testing.Node.Runtime where

import           Enecuum.Prelude

import qualified Data.Map                                                        as Map

import qualified Enecuum.Domain                                                  as D
import qualified Enecuum.Framework.Lens                                          as Lens
import qualified Enecuum.Language                                                as L

import           Enecuum.Core.Testing.Runtime.Types

import qualified Enecuum.Framework.TestData.TestGraph                            as TG
import           Enecuum.Framework.Testing.Environment.TestRuntime
import qualified Enecuum.Framework.Testing.Lens                                  as RLens
import           Enecuum.Framework.Testing.Node.Interpreters.NodeDefinitionModel (runNodeDefinitionModel)
import           Enecuum.Framework.Testing.Types

-- | Creates node runtime.
createEmptyNodeRuntime
  :: LoggerRuntime
  -> Control
  -> D.NodeAddress
  -> IO NodeRuntime
createEmptyNodeRuntime loggerRt networkControl addr = do
  tag <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  graph <- TG.initLGraph
  pure $ NodeRuntime loggerRt networkControl addr tag handle graph

-- | Starts node using NodeDefinitionL.
startNode
  :: TestRuntime
  -> D.NodeAddress
  -> L.NodeDefinitionModel ()
  -> IO NodeRuntime
startNode testRt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime (testRt ^. RLens.loggerRuntime) (testRt ^. RLens.networkControl) nodeAddr
  runNodeDefinitionModel nodeRt scenario
  registerNode (testRt ^. RLens.registry) nodeAddr nodeRt
  pure nodeRt
