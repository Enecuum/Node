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
import           Enecuum.Framework.Testing.Types
import           Enecuum.Framework.Testing.Node.Interpreters.NodeDefinition (runNodeDefinitionL)

-- | Creates node runtime.
createEmptyNodeRuntime
  :: LoggerRuntime
  -> Control
  -> D.NodeAddress
  -> IO NodeRuntime
createEmptyNodeRuntime loggerRt networkControl addr = do
  tag <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  graph <- TG.initTestGraph
  varCounter <- newTMVarIO 0
  state <- newTMVarIO Map.empty
  pure $ NodeRuntime loggerRt networkControl addr tag handle graph varCounter state

-- | Starts node using NodeDefinitionL.
startNode
  :: TestRuntime
  -> D.NodeAddress
  -> L.NodeDefinitionL ()
  -> IO NodeRuntime
startNode testRt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime (testRt ^. RLens.loggerRuntime) (testRt ^. RLens.networkControl) nodeAddr
  runNodeDefinitionL nodeRt scenario
  registerNode (testRt ^. RLens.registry) nodeAddr nodeRt
  pure nodeRt

-- TODO: we need graph creation method.

-- | Starts node using NodeDefinitionL.
-- Uses graph.
startNode'
  :: TestRuntime
  -> D.NodeAddress
  -> (TG.TestGraphVar -> L.NodeDefinitionL ())
  -> IO NodeRuntime
startNode' testRt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime (testRt ^. RLens.loggerRuntime) (testRt ^. RLens.networkControl) nodeAddr
  runNodeDefinitionL nodeRt $ scenario (nodeRt ^. RLens.graph)
  registerNode (testRt ^. RLens.registry) nodeAddr nodeRt
  pure nodeRt

-- | Evaluates node scenario. Does no node registering in the test runtime.
-- Node scenario can't send messages to other nodes (well, it can, but will fail).
evaluateNode
  :: LoggerRuntime
  -> D.NodeAddress
  -> L.NodeDefinitionL a
  -> IO a
evaluateNode loggerRt nodeAddr scenario = do
  control <- createControl
  nodeRt <- createEmptyNodeRuntime loggerRt control nodeAddr
  runNodeDefinitionL nodeRt $ scenario

-- | Evaluates node scenario. Does no node registering in the test runtime.
-- Node scenario can't send messages to other nodes (well, it can, but will fail).
-- Uses graph.
evaluateNode'
  :: LoggerRuntime
  -> D.NodeAddress
  -> (TG.TestGraphVar -> L.NodeDefinitionL a)
  -> IO a
evaluateNode' loggerRt nodeAddr scenario = do
  control <- createControl
  nodeRt <- createEmptyNodeRuntime loggerRt control nodeAddr
  runNodeDefinitionL nodeRt $ scenario (nodeRt ^. RLens.graph)
