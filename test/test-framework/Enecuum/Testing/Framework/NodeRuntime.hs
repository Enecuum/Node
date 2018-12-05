{-# LANGUAGE DuplicateRecordFields #-}

-- | This module contains functions to work with node runtime.
module Enecuum.Testing.Framework.NodeRuntime where

import           Enecuum.Prelude

import qualified Data.Map               as Map

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
--import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Framework.Interpreters as Impl
import qualified Enecuum.Testing.TestRuntime as Impl

-- Bad dependency from TestData.
import qualified Enecuum.TestData.TestGraph as TG

-- | Creates empty node runtime.
createEmptyNodeRuntime :: T.LoggerRuntime -> T.Control -> T.NodeID -> IO T.NodeRuntime
createEmptyNodeRuntime loggerRt networkControl nodeID = do
    tag             <- newTVarIO ("" :: Text)
    rpcServer       <- newEmptyTMVarIO
    serversRegistry <- newEmptyTMVarIO
    graph           <- TG.initTestGraph
    idCounter       <- newTMVarIO 0
    st              <- newTMVarIO Map.empty
    connections     <- newTMVarIO Map.empty
    processes   <- newTMVarIO Map.empty
    pure $ T.NodeRuntime
        { T._loggerRuntime   = loggerRt
        , T._networkControl  = networkControl
        , T._address         = nodeID
        , T._tag             = tag
        , T._rpcServer       = rpcServer
        , T._connections     = connections
        , T._graph           = graph
        , T._idCounter       = idCounter
        , T._state           = st
        , T._serversRegistry = serversRegistry
        , T._processes       = processes
        }

-- | Creates node runtime. Registers in the test runtime.
createNodeRuntime :: T.TestRuntime -> T.NodeID -> IO T.NodeRuntime
createNodeRuntime testRt nodeID = do
    tag         <- newTVarIO ("" :: Text)
    rpcServer   <- newEmptyTMVarIO
    graph       <- TG.initTestGraph
    idCounter   <- newTMVarIO 0
    st          <- newTMVarIO Map.empty
    connections <- newTMVarIO Map.empty
    processes   <- newTMVarIO Map.empty
    let nodeRt = T.NodeRuntime
            { T._loggerRuntime   = testRt ^. RLens.loggerRuntime
            , T._networkControl  = testRt ^. RLens.networkControl
            , T._address         = nodeID
            , T._tag             = tag
            , T._rpcServer       = rpcServer
            , T._connections     = connections
            , T._graph           = graph
            , T._idCounter       = idCounter
            , T._state           = st
            , T._serversRegistry = testRt ^. RLens.serversRegistry
            , T._processes       = processes
            }
    Impl.registerNode (testRt ^. RLens.registry) nodeID nodeRt
    pure nodeRt

-- | Starts node using NodeDefinitionL.
startNode :: T.TestRuntime -> D.Address -> L.NodeDefinitionL () -> IO T.NodeRuntime
startNode testRt nodeAddr scenario = do
    nodeRt <- createNodeRuntime testRt nodeAddr
    Impl.runNodeDefinitionL nodeRt scenario
    pure nodeRt

-- | Starts node using NodeDefinitionL.
-- Uses graph.
startNodeWithGraph :: T.TestRuntime -> D.Address -> (TG.TestGraphVar -> L.NodeDefinitionL ()) -> IO T.NodeRuntime
startNodeWithGraph testRt nodeAddr scenario = do
    nodeRt <- createNodeRuntime testRt nodeAddr
    Impl.runNodeDefinitionL nodeRt $ scenario (nodeRt ^. RLens.graph)
    pure nodeRt

-- | Evaluates node scenario. Does no node registering in the test runtime.
-- Node scenario can't send messages to other nodes (well, it can, but will fail).
evaluateNode :: T.LoggerRuntime -> D.Address -> L.NodeDefinitionL a -> IO a
evaluateNode loggerRt nodeAddr scenario = do
    control <- Impl.createControl
    nodeRt  <- createEmptyNodeRuntime loggerRt control nodeAddr
    Impl.runNodeDefinitionL nodeRt scenario

-- | Evaluates node scenario. Does no node registering in the test runtime.
-- Node scenario can't send messages to other nodes (well, it can, but will fail).
-- Uses graph.
evaluateNodeWithGraph :: T.LoggerRuntime -> D.Address -> (TG.TestGraphVar -> L.NodeDefinitionL a) -> IO a
evaluateNodeWithGraph loggerRt nodeAddr scenario = do
    control <- Impl.createControl
    nodeRt  <- createEmptyNodeRuntime loggerRt control nodeAddr
    Impl.runNodeDefinitionL nodeRt $ scenario (nodeRt ^. RLens.graph)
