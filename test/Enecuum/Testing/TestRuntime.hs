-- | This module contains functions to maintain a test runtime,
-- including virtual network and the network environment thread.

module Enecuum.Testing.TestRuntime where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain as D

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.Core.LoggerRuntime as T
import qualified Enecuum.Testing.RLens as RLens

-- | Worker for the network environment thread.
-- Serves control request:
-- - Relay RPC request from one node to another.
networkWorker :: T.Control -> T.NodesRegistry -> IO ()
networkWorker control registry = go 0
  where

  go :: Integer -> IO ()
  go iteration = act iteration >> go (iteration + 1)

  act :: Integer -> IO ()
  act _ = do
    controlReq <- atomically $ takeTMVar $ control ^. RLens.request
    case controlReq of
      T.RelayRpcReq _ toAddr req -> makeRpcRequestRelay toAddr req
      _ -> error "Unkwnown ControlResponse."

  makeRpcRequestRelay toAddr req = do
        nodes       <- atomically $ takeTMVar registry
        controlResp <- case Map.lookup toAddr nodes of
            Nothing -> pure $ T.AsErrorResp
                            $ "Can't relay to " +| D.formatAddress toAddr |+ ": node not found."
            Just toNodeRt -> controlRequest toNodeRt $ T.RpcReq req
        atomically $ putTMVar (control ^. RLens.response) controlResp
        atomically $ putTMVar registry nodes

createControl :: IO T.Control
createControl = T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO

-- | Creates test runtime.
-- Starts network environment thread.
createTestRuntime :: IO T.TestRuntime
createTestRuntime = do
  loggerRt <- T.createLoggerRuntime
  registry <- newTMVarIO Map.empty
  control  <- createControl
  tId      <- forkIO $ networkWorker control registry
  pure $ T.TestRuntime loggerRt tId control registry

-- | Registers a node.
registerNode
  :: T.NodesRegistry
  -> D.Address
  -> T.NodeRuntime
  -> IO ()
registerNode registry addr nodeRt = do
  nodes <- atomically $ takeTMVar registry
  case Map.lookup addr nodes of
    Just _ -> error $ "Node is already registered: " +| D.formatAddress addr |+ ""
    Nothing -> atomically $ putTMVar registry $ Map.insert addr nodeRt nodes

-- | Lookups a node from the registry.
findNode
  :: T.NodesRegistry
  -> D.Address
  -> IO (Maybe T.NodeRuntime)
findNode registry addr = do
  nodes <- atomically $ readTMVar registry
  pure $ Map.lookup addr nodes

-- | Sends a control request to the node (inside STM).
putControlRequest :: T.NodeRuntime -> T.ControlRequest -> STM ()
putControlRequest nodeRt controlReq = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  putTMVar (rpcServer ^. RLens.control . RLens.request) controlReq

-- | Takes a control response from the node (inside STM).
takeControlResponse :: T.NodeRuntime -> STM T.ControlResponse
takeControlResponse nodeRt = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  takeTMVar (rpcServer ^. RLens.control . RLens.response)

-- | Sends control request and waits for control response.
controlRequest :: T.NodeRuntime -> T.ControlRequest -> IO T.ControlResponse
controlRequest nodeRt controlReq = do
  atomically $ putControlRequest nodeRt controlReq
  atomically $ takeControlResponse nodeRt

-- | Sends some RPC request to the node.
sendRequest' :: T.NodesRegistry -> D.Address -> D.RpcRequest -> IO (Either Text D.RpcResponse)
sendRequest' registry toAddr req = findNode registry toAddr >>= \case
  Nothing -> pure $ Left $ "Destination node is not registered: " +| D.formatAddress toAddr |+ ""
  Just nodeRt -> do
    controlResp <- controlRequest nodeRt $ T.RpcReq req
    case controlResp of
      T.AsRpcResp rpcResp -> pure $ Right rpcResp
      T.AsErrorResp err   -> pure $ Left $ "Control error got: " +| err |+ "."

-- | Sends some RPC request to the node.
sendRequest
  :: T.TestRuntime
  -> D.Address
  -> D.RpcRequest
  -> IO (Either Text D.RpcResponse)
sendRequest testRt = sendRequest' (testRt ^. RLens.registry)
