module Enecuum.Framework.Testing.Node.Interpreters.Networking where

import Enecuum.Prelude

import           Control.Monad.Free                 (foldFree)

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Node.Interpreters.Network as Impl
import qualified Enecuum.Core.Testing.Runtime.Interpreters as Impl
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Legacy.Service.Network.Base

data TestConnection = TestConnection (MVar NodeRuntime) D.Connection 

instance D.ConnectionClass TestConnection where
    -- :: Monad m => ConnectionConfig -> m (Maybe a)
  openConnection (D.ConnectionConfig (ConnectInfo host port)) = error ""


  -- :: Monad m => a -> m ()
  closeConnection (TestConnection rt conn) = pure ()
  sendRequest (TestConnection rt conn) req = do
    var <- readMVar rt
    relayRequest var conn req

-- | Relay request from this node to the network environment.
relayRequest
  :: NodeRuntime
  -> D.Connection
  -> RpcRequest
  -> IO (Either Text RpcResponse)
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
interpretNetworkingL :: NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL nodeRt (L.OpenConnection cfg next) = do
  rt <- newMVar nodeRt
  pure $ next $ Just $ D.NetworkConnection $ TestConnection
    rt $ D.Connection (nodeRt ^. RLens.address) (cfg ^. Lens.address)

interpretNetworkingL nodeRt (L.CloseConnection _ next) =
  pure $ next ()

interpretNetworkingL nodeRt (L.SendRequest (D.NetworkConnection conn) req next) = do
  next <$> D.sendRequest conn req

interpretNetworkingL nodeRt (L.EvalNetwork networkAction next) = do
  next <$> Impl.runNetworkL nodeRt networkAction

interpretNetworkingL nodeRt (L.EvalCoreEffectNetworkingF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs networking language.
runNetworkingL :: NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingL nodeRt)
