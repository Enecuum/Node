module Enecuum.Testing.Framework.Interpreters.Networking where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.RLens                          as RLens
import qualified Enecuum.Testing.Types                          as T
import qualified Enecuum.Testing.Core.Interpreters              as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Network as Impl

-- | Relay request from this node to the network environment for target server node.
relayRequest'
  :: T.NodeRuntime
  -> D.Address
  -> D.RpcRequest
  -> IO (Either Text D.RpcResponse)
relayRequest' nodeRt to req = do
  atomically
      $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
      $ T.RelayRpcReq (nodeRt ^. RLens.address) to req
  controlResponse <- atomically
      $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
  case controlResponse of
    T.AsRpcResp rpcResponse -> pure $ Right rpcResponse
    T.AsErrorResp err       -> pure $ Left err
    _                       -> error "Invalid network control result."

-- | Relay message from this node to the network environment for target server node.
relayMessage'
  :: T.NodeRuntime
  -> T.NodeConnection
  -> D.RawData
  -> IO (Either Text ())
relayMessage' nodeRt toNodeConnection msg = do
      atomically
          $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
          $ T.RelayMessageReq (nodeRt ^. RLens.address) (toNodeConnection ^. RLens.nodeID) (toNodeConnection ^. RLens.bindingAddress) msg
      controlResponse <- atomically
          $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
      case controlResponse of
        T.AsSuccessResp    -> pure $ Right ()
        T.AsErrorResp err  -> pure $ Left err
        _                  -> error "Invalid network control result."

-- | Send message to the connection.
sendMessageToConnection
  :: T.NodeRuntime
  -> D.NetworkConnection
  -> D.RawData
  -> IO (Either Text ())
sendMessageToConnection nodeRt networkConnection msg = do
  connections <- atomically $ readTMVar $ nodeRt ^. RLens.connections
  -- Checking is connection alive.
  case Map.lookup (networkConnection ^. Lens.address) connections of
    Nothing -> pure $ Left $ "Failed sending message."
    Just runtimeConnection -> relayMessage' nodeRt runtimeConnection msg



-- | Interpret NetworkingL language.
interpretNetworkingL :: T.NodeRuntime -> L.NetworkingF a -> IO a

interpretNetworkingL nodeRt (L.SendRpcRequest toAddr req next) =
  next <$> relayRequest' nodeRt toAddr req

interpretNetworkingL nodeRt (L.SendMessage conn msg next) = do
  eResult <- sendMessageToConnection nodeRt conn msg
  case eResult of
    Right _ -> pure $ next ()
    Left err -> error err

interpretNetworkingL nodeRt (L.EvalNetwork networkAction next) =
  next <$> Impl.runNetworkL nodeRt networkAction

interpretNetworkingL nodeRt (L.EvalCoreEffectNetworkingF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs networking language.
runNetworkingL :: T.NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingL nodeRt)
