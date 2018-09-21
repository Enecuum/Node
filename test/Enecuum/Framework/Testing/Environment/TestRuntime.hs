-- | This module contains functions to maintain a test runtime,
-- including virtual network and the network environment thread.

module Enecuum.Framework.Testing.Environment.TestRuntime where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D

import           Enecuum.Framework.Testing.Types as T
import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens
import qualified Enecuum.Framework.Domain.Networking as N
import           Enecuum.Framework.Domain.RpcMessages


-- TODO: consider to use forever.
-- | Worker for the network environment thread.
-- Serves control request:
-- - Relay RPC request from one node to another.
networkWorker :: Control -> NodesRegistry -> IO ()
networkWorker control registry = go 0
  where

  go :: Integer -> IO ()
  go iteration = act iteration >> go (iteration + 1)

  act :: Integer -> IO ()
  act _ = do
    controlReq <- atomically $ takeTMVar $ control ^. RLens.request
    case controlReq of
      RelayRpcRequest _ toAddr req -> makeRpcRequestRelay toAddr req
      _ -> error "Unkwnown ControlResponse."

  makeRpcRequestRelay toAddr req = do
        nodes       <- atomically $ takeTMVar registry
        controlResp <- case Map.lookup toAddr nodes of
            Nothing -> pure $ AsErrorResponse
                            $ "Can't relay to " +| N.infoToText toAddr |+ ": node not found."
            Just toNodeRt -> controlRequest toNodeRt $ T.RpcRequest req
        atomically $ putTMVar (control ^. RLens.response) controlResp
        atomically $ putTMVar registry nodes

createControl :: IO Control
createControl = Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO

-- | Creates test runtime.
-- Starts network environment thread.
createTestRuntime :: IO TestRuntime
createTestRuntime = do
  loggerRt <- createLoggerRuntime
  registry <- newTMVarIO Map.empty
  control  <- createControl
  tId      <- forkIO $ networkWorker control registry
  pure $ TestRuntime loggerRt tId control registry

-- | Registers a node.
registerNode
  :: NodesRegistry
  -> D.NodeAddress
  -> NodeRuntime
  -> IO ()
registerNode registry addr nodeRt = do
  nodes <- atomically $ takeTMVar registry
  case Map.lookup addr nodes of
    Just _ -> error $ "Node is already registered: " +| N.infoToText addr |+ ""
    Nothing -> atomically $ putTMVar registry $ Map.insert addr nodeRt nodes

-- | Lookups a node from the registry.
findNode
  :: NodesRegistry
  -> D.NodeAddress
  -> IO (Maybe NodeRuntime)
findNode registry addr = do
  nodes <- atomically $ readTMVar registry
  pure $ Map.lookup addr nodes

-- | Sends a control request to the node (inside STM).
putControlRequest :: NodeRuntime -> ControlRequest -> STM ()
putControlRequest nodeRt controlReq = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  putTMVar (rpcServer ^. RLens.control . RLens.request) controlReq

-- | Takes a control response from the node (inside STM).
takeControlResponse :: NodeRuntime -> STM ControlResponse
takeControlResponse nodeRt = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  takeTMVar (rpcServer ^. RLens.control . RLens.response)

-- | Sends control request and waits for control response.
controlRequest :: NodeRuntime -> ControlRequest -> IO ControlResponse
controlRequest nodeRt controlReq = do
  atomically $ putControlRequest nodeRt controlReq
  atomically $ takeControlResponse nodeRt

-- | Sends some RPC request to the node.
sendRequest' :: NodesRegistry -> D.NodeAddress -> RpcRequest -> IO (Either Text RpcResponse)
sendRequest' registry toAddr req = findNode registry toAddr >>= \case
  Nothing -> pure $ Left $ "Destination node is not registered: " +| N.infoToText toAddr |+ ""
  Just nodeRt -> do
    controlResp <- controlRequest nodeRt $ T.RpcRequest req
    case controlResp of
      AsRpcResponse rpcResp -> pure $ Right rpcResp
      AsErrorResponse err   -> pure $ Left $ "Control error got: " +| err |+ "."

-- | Sends some RPC request to the node.
sendRequest
  :: TestRuntime
  -> D.NodeAddress
  -> RpcRequest
  -> IO (Either Text RpcResponse)
sendRequest testRt = sendRequest' (testRt ^. RLens.registry) 
