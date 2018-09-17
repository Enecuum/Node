module Enecuum.Framework.Testing.Node.Interpreters.Networking where

import Enecuum.Prelude

import           Control.Monad.Free                 (foldFree)

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Node.Interpreters.NetworkModel as Impl
import qualified Enecuum.Core.Testing.Runtime.Interpreters as Impl

-- | Relay request from this node to the network environment.
relayRequest
  :: NodeRuntime
  -> D.Connection
  -> D.RpcRequest
  -> IO (D.RpcResult D.RpcResponse)
relayRequest nodeRt conn req = do
  atomically
      $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
      $ RelayRpcRequest (conn ^. Lens.clientAddress) (conn ^. Lens.serverAddress) req
  controlResponse <- atomically
      $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
  case controlResponse of
    AsRpcResponse rpcResponse -> pure $ Right rpcResponse
    AsErrorResponse err       -> pure $ Left err

-- | Interpret NetworkingL language.
interpretNetworkingL
  :: NodeRuntime
  -> L.NetworkingF a
  -> IO a

interpretNetworkingL nodeRt (L.OpenConnection cfg next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "OpenConnection cfg"
  pure $ next $ Just $ D.Connection (nodeRt ^. RLens.address) (cfg ^. Lens.address)

interpretNetworkingL nodeRt (L.CloseConnection _ next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ next <$> L.logInfo "CloseConnection conn"

interpretNetworkingL nodeRt (L.SendRequest conn req next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "SendRequest conn req"
  next <$> relayRequest nodeRt conn req

interpretNetworkingL nodeRt (L.EvalNetwork networkAction next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "Eval Network"
  next <$> Impl.runNetworkModel nodeRt networkAction

interpretNetworkingL nodeRt (L.EvalCoreEffectNetworkingF coreEffect next) =
  next <$> Impl.runCoreEffectModel (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs networking language.
runNetworkingL :: NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingL nodeRt)
