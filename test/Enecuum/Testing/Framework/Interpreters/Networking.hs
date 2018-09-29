module Enecuum.Testing.Framework.Interpreters.Networking where

import Enecuum.Prelude

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.RLens                          as RLens
import qualified Enecuum.Testing.Types                          as T
import qualified Enecuum.Testing.Core.Interpreters              as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Network as Impl

-- | Relay request from this node to the network environment.
relayRequest
  :: T.NodeRuntime
  -> D.Address
  -> D.Address
  -> D.RpcRequest
  -> IO (Either Text D.RpcResponse)
relayRequest nodeRt from to req = do
  atomically
      $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
      $ T.RelayRpcReq from to req
  controlResponse <- atomically
      $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
  case controlResponse of
    T.AsRpcResp rpcResponse -> pure $ Right rpcResponse
    T.AsErrorResp err       -> pure $ Left err

-- | Interpret NetworkingL language.
interpretNetworkingL :: T.NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL nodeRt (L.OpenConnection cfg next) = do
  -- rt <- newMVar nodeRt
  -- pure $ next $ Just $ D.NetworkConnection $ TestConnection
    -- rt $ Connection (nodeRt ^. RLens.address) (cfg ^. Lens.address)
  error "OpenConnection not implemented."

interpretNetworkingL nodeRt (L.CloseConnection _ next) =
  -- pure $ next ()
  error "CloseConnection not implemented."

-- interpretNetworkingL nodeRt (L.Send (D.NetworkConnection conn) req next) = do
--   -- next <$> D.sendRequest conn req
--   error "Send not implemented."

interpretNetworkingL nodeRt (L.SendRpcRequest toAddr req next) = do
  res <- relayRequest nodeRt (nodeRt ^. RLens.address) toAddr req
  pure $ next res

interpretNetworkingL nodeRt (L.EvalNetwork networkAction next) =
  next <$> Impl.runNetworkL nodeRt networkAction

interpretNetworkingL nodeRt (L.EvalCoreEffectNetworkingF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs networking language.
runNetworkingL :: T.NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingL nodeRt)
