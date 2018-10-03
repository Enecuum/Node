module Enecuum.Testing.Framework.Interpreters.Node where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                                    as D
import qualified Enecuum.Language                                  as L

import qualified Enecuum.Testing.RLens                             as RLens
import qualified Enecuum.Testing.Types                             as T
import qualified Enecuum.Testing.Core.Interpreters                 as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Networking as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State      as Impl
import           Enecuum.Core.HGraph.Interpreters.IO               (runHGraphIO)

-- | Establish connection with the server through test environment.
-- TODO: check if connection exists.
establishConnection
  :: T.NodeRuntime
  -> D.Address
  -> IO (Either Text T.BindedServer)
establishConnection nodeRt toAddress = do
  atomically
      $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
      $ T.RelayEstablishConnectionReq toAddress
  controlResponse <- atomically
      $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
  case controlResponse of
    T.AsConnectionAccepted bindedServer -> pure $ Right bindedServer
    T.AsErrorResp msg                   -> pure $ Left $ "Failed to establish connection: " <> msg
    _                                   -> error "Invalid network control result."


-- | Register connection in the node connections list.
-- TODO: check if connection exists.
registerConnection
  :: T.NodeRuntime
  -> T.BindedServer
  -> IO ()
registerConnection nodeRt bindedServer = atomically $ do
    connections <- takeTMVar (nodeRt ^. RLens.connections)
    let newConnection = T.NodeConnection T.Server bindedServer
    let newConnections = Map.insert (bindedServer ^. RLens.address) newConnection connections
    putTMVar (nodeRt ^. RLens.connections) newConnections


-- | Interpret NodeL.
interpretNodeL :: T.NodeRuntime -> L.NodeF a -> IO a

interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
  next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL nodeRt (L.EvalGraphIO gr act next) =
  next <$> runHGraphIO gr act

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

interpretNodeL nodeRt (L.OpenConnection serverAddress handlers next) = do
  eAcceptedConnection <- establishConnection nodeRt serverAddress
  case eAcceptedConnection of
    Left err -> error err
    Right bindedServer -> do
      registerConnection nodeRt bindedServer
      pure $ next $ D.NetworkConnection $ bindedServer ^. RLens.address

interpretNodeL nodeRt (L.CloseConnection _ next) =
  error "CloseConnection not implemented."

-- | Runs node language.
runNodeL :: T.NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
