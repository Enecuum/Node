module Enecuum.Framework.Testing.Node.Interpreters.Networking where

import Enecuum.Prelude

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types

-- | Relay request from this node to the network environment.
relayRequest
  :: NodeRuntime
  -> D.Connection
  -> D.RpcRequest
  -> Eff L.NetworkModel (D.RpcResult D.RpcResponse)
relayRequest nodeRt conn req = do
  safeIO
    $ atomically
    $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
    $ RelayRpcRequest (conn ^. Lens.clientAddress) (conn ^. Lens.serverAddress) req
  controlResponse <- safeIO
    $ atomically
    $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
  case controlResponse of
    AsRpcResponse rpcResponse -> pure $ Right rpcResponse
    AsErrorResponse err       -> pure $ Left err

-- | Interpret NetworkingL language.
interpretNetworkingL
  :: NodeRuntime
  -> L.NetworkingL a
  -> Eff L.NetworkModel a
interpretNetworkingL nodeRt (L.OpenConnection cfg) = do
  L.logInfo "OpenConnection cfg"
  pure $ Just $ D.Connection (nodeRt ^. RLens.address) (cfg ^. Lens.address)
interpretNetworkingL _ (L.CloseConnection _) = do
  L.logInfo "CloseConnection conn"
  pure ()
interpretNetworkingL nodeRt (L.SendRequest conn req) = do
  L.logInfo "SendRequest conn req"
  relayRequest nodeRt conn req
interpretNetworkingL _ (L.EvalNetwork networkAction) = do
  L.logInfo "Eval Network"
  networkAction
