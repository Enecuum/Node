module Enecuum.Framework.Testing.Environment.TestRuntime where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens

createTestRuntime :: IO TestRuntime
createTestRuntime = do
  loggerRt <- createLoggerRuntime
  nodes <- newTMVarIO Map.empty
  pure $ TestRuntime loggerRt nodes

registerNode
  :: TestRuntime
  -> D.NodeAddress
  -> NodeRuntime
  -> IO ()
registerNode testRt addr nodeRt = do
  nodes <- atomically $ takeTMVar $ testRt ^. RLens.nodes
  case Map.lookup addr nodes of
    Just _ -> error "Node is already registered."
    Nothing -> atomically
      $ putTMVar (testRt ^. RLens.nodes)
      $ Map.insert addr nodeRt nodes

findNode
  :: TestRuntime
  -> D.NodeAddress
  -> IO (Maybe NodeRuntime)
findNode testRt addr = do
  nodes <- atomically $ readTMVar $ testRt ^. RLens.nodes
  pure $ Map.lookup addr nodes

putControlRequest :: NodeRuntime -> ControlRequest -> STM ()
putControlRequest nodeRt controlReq = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  putTMVar (rpcServer ^. RLens.control . RLens.request) controlReq

takeControlResponse :: NodeRuntime -> STM ControlResponse
takeControlResponse nodeRt = do
  rpcServer <- readTMVar $ nodeRt ^. RLens.rpcServer
  takeTMVar (rpcServer ^. RLens.control . RLens.response)

sendRequest
  :: D.RpcMethod () req resp
  => TestRuntime
  -> D.NodeAddress
  -> req
  -> IO (Either Text resp)
sendRequest testRt toAddr req = findNode testRt toAddr >>= \case
  Nothing -> pure $ Left $ "Destination node is not registered: " +| toAddr |+ ""
  Just nodeRt -> do
    -- TODO: check whether this should be a single transaction (probably not)
    atomically $ putControlRequest nodeRt $ ControlRpcRequest $ D.toRpcRequest () req
    controlResp <- atomically $ takeControlResponse nodeRt
    case controlResp of
      ControlRpcResponse rpcResp -> pure $ D.fromRpcResponse () rpcResp
      _ -> pure $ Left "Unknown type of ControlRpcResponse got."
