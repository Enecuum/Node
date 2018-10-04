-- | This module contains functions to maintain a test runtime,
-- including virtual network and the network environment thread.

module Enecuum.Testing.TestRuntime where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain as D

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.Core.LoggerRuntime as T
import qualified Enecuum.Testing.RLens as RLens

-- TODO: Test runtime is too complex. Maybe, it can be done much better.

-- | Worker for the network environment thread.
-- Serves control request:
-- - Relay RPC request from one node to another.
networkWorker :: T.Control -> T.NodesRegistry -> T.ServersRegistry -> IO ()
networkWorker networkControl registry serversRegistry = go 0
  where

  go :: Integer -> IO ()
  go iteration = act iteration >> go (iteration + 1)

  act :: Integer -> IO ()
  act _ = do
    controlReq <- atomically $ takeTMVar $ networkControl ^. RLens.request
    case controlReq of
      T.RelayRpcReq _ toAddr req -> relayRpcRequestToNode toAddr req
      T.RelayEstablishConnectionReq serverAddr -> relayEstablishConnection serverAddr
      -- T.RelayMessageReq _ nodeID bindingAddr msg       -> relayMessageToNode nodeID bindingAddr msg
      _ -> error "Unkwnown control request."

  relayRpcRequestToNode toAddr req = do
        nodes       <- atomically $ readTMVar registry
        controlResp <- case Map.lookup toAddr nodes of
            Nothing -> pure $ T.AsErrorResp $ "Can't relay RPC to " +| D.formatAddress toAddr |+ ": node not found."
            Just toNodeRt -> controlRpcRequest toNodeRt $ T.RpcReq req
        atomically $ putTMVar (networkControl ^. RLens.response) controlResp

  relayEstablishConnection serverAddr = do
      mbServerHandler <- getServerHandler serverAddr serversRegistry
      controlResp <- case mbServerHandler of
          Nothing           -> pure $ T.AsErrorResp $ "Can't establish connection. Server node " +|| serverAddr ||+ " not found."
          Just serverHandle -> controlRequest (serverHandle ^. RLens.control) T.EstablishConnectionReq
      atomically $ putTMVar (networkControl ^. RLens.response) controlResp


getServerHandler :: D.Address -> T.ServersRegistry -> IO (Maybe T.ServerHandle)
getServerHandler serverAddress serversRegistry = do
    servers <- atomically $ readTMVar serversRegistry
    pure $ Map.lookup serverAddress servers

createControl :: IO T.Control
createControl = T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO

-- | Creates test runtime.
-- Starts network environment thread.
createTestRuntime :: IO T.TestRuntime
createTestRuntime = do
  loggerRt <- T.createLoggerRuntime
  registry <- newTMVarIO Map.empty
  serversRegistry <- newTMVarIO Map.empty
  control  <- createControl
  tId      <- forkIO $ networkWorker control registry serversRegistry
  pure $ T.TestRuntime loggerRt tId control registry serversRegistry

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

-- | Sends control request and waits for control response.
controlRequest :: T.Control -> T.ControlRequest -> IO T.ControlResponse
controlRequest control controlReq = do
  atomically $ putTMVar (control ^. RLens.request) controlReq
  atomically $ takeTMVar (control ^. RLens.response)

-- | Sends control request and waits for control response.
controlRpcRequest :: T.NodeRuntime -> T.ControlRequest -> IO T.ControlResponse
controlRpcRequest nodeRt controlReq = do
  rpcServer <- atomically $ readTMVar $ nodeRt ^. RLens.rpcServer
  controlRequest (rpcServer ^. RLens.control) controlReq

-- | Sends some RPC request to the node.
sendRequest' :: T.NodesRegistry -> D.Address -> D.RpcRequest -> IO (Either Text D.RpcResponse)
sendRequest' registry toAddr req = findNode registry toAddr >>= \case
  Nothing -> pure $ Left $ "Destination node is not registered: " +| D.formatAddress toAddr |+ ""
  Just nodeRt -> do
    controlResp <- controlRpcRequest nodeRt $ T.RpcReq req
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
