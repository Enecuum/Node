-- | This module contains functions to work with node runtime.
module Enecuum.Testing.Framework.NodeRuntime where

import           Enecuum.Prelude

import qualified Data.Map               as Map

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Framework.Interpreters as Impl
import qualified Enecuum.Testing.TestRuntime as Impl

-- Bad dependency from TestData.
import qualified Enecuum.TestData.TestGraph as TG

-- | Creates node runtime.
createEmptyNodeRuntime
  :: T.LoggerRuntime
  -> T.Control
  -> D.Address
  -> IO T.NodeRuntime
createEmptyNodeRuntime loggerRt networkControl addr = do
  tag <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  graph <- TG.initTestGraph
  varCounter <- newTMVarIO 0
  state <- newTMVarIO Map.empty
  pure $ T.NodeRuntime loggerRt networkControl addr tag handle graph varCounter state

-- | Starts node using NodeDefinitionL.
startNode
  :: T.TestRuntime
  -> D.Address
  -> L.NodeDefinitionL ()
  -> IO T.NodeRuntime
startNode testRt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime (testRt ^. RLens.loggerRuntime) (testRt ^. RLens.networkControl) nodeAddr
  Impl.registerNode (testRt ^. RLens.registry) nodeAddr nodeRt
  Impl.runNodeDefinitionL nodeRt scenario
  pure nodeRt

-- | Starts node using NodeDefinitionL.
-- Uses graph.
startNodeWithGraph
  :: T.TestRuntime
  -> D.Address
  -> (TG.TestGraphVar -> L.NodeDefinitionL ())
  -> IO T.NodeRuntime
startNodeWithGraph testRt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime (testRt ^. RLens.loggerRuntime) (testRt ^. RLens.networkControl) nodeAddr
  Impl.registerNode (testRt ^. RLens.registry) nodeAddr nodeRt
  Impl.runNodeDefinitionL nodeRt $ scenario (nodeRt ^. RLens.graph)
  pure nodeRt

-- | Evaluates node scenario. Does no node registering in the test runtime.
-- Node scenario can't send messages to other nodes (well, it can, but will fail).
evaluateNode
  :: T.LoggerRuntime
  -> D.Address
  -> L.NodeDefinitionL a
  -> IO a
evaluateNode loggerRt nodeAddr scenario = do
  control <- Impl.createControl
  nodeRt <- createEmptyNodeRuntime loggerRt control nodeAddr
  Impl.runNodeDefinitionL nodeRt scenario

-- | Evaluates node scenario. Does no node registering in the test runtime.
-- Node scenario can't send messages to other nodes (well, it can, but will fail).
-- Uses graph.
evaluateNodeWithGraph
  :: T.LoggerRuntime
  -> D.Address
  -> (TG.TestGraphVar -> L.NodeDefinitionL a)
  -> IO a
evaluateNodeWithGraph loggerRt nodeAddr scenario = do
  control <- Impl.createControl
  nodeRt <- createEmptyNodeRuntime loggerRt control nodeAddr
  Impl.runNodeDefinitionL nodeRt $ scenario (nodeRt ^. RLens.graph)
