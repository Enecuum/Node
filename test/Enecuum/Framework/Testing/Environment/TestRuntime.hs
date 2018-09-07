module Enecuum.Framework.Testing.Environment.TestRuntime where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens

-- TODO: consider to use forever.
networkWorker :: Control -> NodesRegistry -> IO ()
networkWorker control registry = go 0
  where

  go :: Integer -> IO ()
  go iteration = act iteration >> go (iteration + 1)

  act :: Integer -> IO ()
  act _ = do
    controlReq <- atomically $ takeTMVar $ control ^. RLens.request
    case controlReq of
      RelayRpcRequest _ toAddr req -> do
        nodes <- atomically $ takeTMVar registry
        controlResp <- case Map.lookup toAddr nodes of
            Nothing -> pure $ AsErrorResponse
                            $ "Can't relay to " +| toAddr |+ ": node not found."
            Just toNodeRt -> controlRequest toNodeRt $ RpcRequest req
        atomically $ putTMVar (control ^. RLens.response)
                   $ controlResp
        atomically $ putTMVar registry nodes
      _ -> error "Unkwnown ControlResponse."

createTestRuntime :: IO TestRuntime
createTestRuntime = do
  loggerRt <- createLoggerRuntime
  registry <- newTMVarIO Map.empty
  control  <- Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId      <- forkIO $ networkWorker control registry
  pure $ TestRuntime loggerRt tId control registry

registerNode
  :: NodesRegistry
  -> D.NodeAddress
  -> NodeRuntime
  -> IO ()
registerNode registry addr nodeRt = do
  nodes <- atomically $ takeTMVar registry
  case Map.lookup addr nodes of
    Just _ -> error $ "Node is already registered: " +| addr |+ ""
    Nothing -> atomically $ putTMVar registry $ Map.insert addr nodeRt nodes

findNode
  :: NodesRegistry
  -> D.NodeAddress
  -> IO (Maybe NodeRuntime)
findNode registry addr = do
  nodes <- atomically $ readTMVar registry
  pure $ Map.lookup addr nodes

putControlRequest :: NodeRuntime -> ControlRequest -> STM ()
putControlRequest nodeRt controlReq = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  putTMVar (rpcServer ^. RLens.control . RLens.request) controlReq

takeControlResponse :: NodeRuntime -> STM ControlResponse
takeControlResponse nodeRt = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  takeTMVar (rpcServer ^. RLens.control . RLens.response)

controlRequest :: NodeRuntime -> ControlRequest -> IO ControlResponse
controlRequest nodeRt controlReq = do
  atomically $ putControlRequest nodeRt controlReq
  atomically $ takeControlResponse nodeRt

sendRequest'
  :: D.RpcMethod () req resp
  => NodesRegistry
  -> D.NodeAddress
  -> req
  -> IO (Either Text resp)
sendRequest' registry toAddr req = findNode registry toAddr >>= \case
  Nothing -> pure $ Left $ "Destination node is not registered: " +| toAddr |+ ""
  Just nodeRt -> do
    controlResp <- controlRequest nodeRt $ RpcRequest $ D.toRpcRequest () req
    case controlResp of
      AsRpcResponse rpcResp -> pure $ D.fromRpcResponse () rpcResp
      AsErrorResponse err   -> pure $ Left $ "Control error got: " +| err |+ "."

sendRequest
  :: D.RpcMethod () req resp
  => TestRuntime
  -> D.NodeAddress
  -> req
  -> IO (Either Text resp)
sendRequest testRt = sendRequest' (testRt ^. RLens.registry)
