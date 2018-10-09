module Enecuum.Testing.Framework.Interpreters.Node where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                                    as D
import qualified Enecuum.Language                                  as L
import qualified Enecuum.Framework.Lens                            as Lens

import qualified Enecuum.Testing.RLens                             as RLens
import qualified Enecuum.Testing.Types                             as T
import qualified Enecuum.Testing.Core.Interpreters                 as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Networking as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State      as Impl
import           Enecuum.Core.HGraph.Interpreters.IO               (runHGraphIO)
import           Enecuum.Testing.Framework.Internal.TcpLikeServerWorker  (startNodeTcpLikeWorker)
import           Enecuum.Testing.Framework.Internal.TcpLikeServerBinding (bindServer, registerConnection, closeConnection)
import           Enecuum.Testing.TestRuntime                             (controlRequest)

import qualified Enecuum.Framework.MsgHandler.Interpreter as Impl (runMsgHandlerL)


-- | Establish connection with the server through test environment.
-- TODO: check if connection exists.
establishConnection :: T.NodeRuntime -> D.Address -> IO (Either Text T.BindedServer)
establishConnection nodeRt toAddress = do
    atomically $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request) $ T.RelayEstablishConnectionReq toAddress
    controlResponse <- atomically $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
    case controlResponse of
        T.AsConnectionAccepted bindedServer -> pure $ Right bindedServer
        T.AsErrorResp          msg -> pure $ Left $ "Failed to establish connection: " <> msg
        _ -> error "Invalid network control result."

-- | Send client connection to the binded server.
sendClientConnection :: T.BindedServer -> T.BindedServer -> IO (Either Text ())
sendClientConnection bindedServer bindedClientsideServer = do
    controlResp <- controlRequest (bindedServer ^. RLens.handle . RLens.control)
        $ T.AcceptBackConnectionReq bindedClientsideServer
    case controlResp of
        T.AsSuccessResp -> pure $ Right ()
        _               -> pure $ Left "Unknown control response."

-- | Interpret NodeL.
interpretNodeL :: T.NodeRuntime -> L.NodeF a -> IO a

interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL _      (L.EvalGraphIO gr act next             ) = next <$> runHGraphIO gr act

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) = next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

interpretNodeL nodeRt (L.OpenConnection serverAddress handlersF next) = do
  -- Asking the server to accept connection
    eBindedServer <- establishConnection nodeRt serverAddress
    case eBindedServer of
        Left  err          -> error err
        Right bindedServer -> do
            let bindedServerConnection = D.NetworkConnection $ bindedServer ^. RLens.address

            -- Collecting hanlders for this connection
            tHandlers <- atomically $ newTVar mempty
            Impl.runMsgHandlerL tHandlers handlersF
            handlers <- readTVarIO tHandlers

            -- Starting client side connection worker & registering this connection.
            let thisHost = nodeRt ^. RLens.address . Lens.host
            clientsideServerHandle <- startNodeTcpLikeWorker (runNodeL nodeRt)
                                                             nodeRt
                                                             handlers
                                                             (Just bindedServerConnection)
            bindedClientsideServer <- bindServer nodeRt thisHost T.Client clientsideServerHandle

            -- Sending client side connection to server.
            eResult                <- sendClientConnection bindedServer bindedClientsideServer
            case eResult of
                Left  err -> error err
                Right _   -> do
                    -- registering bindedServer.
                    registerConnection nodeRt bindedServer
                    pure $ next bindedServerConnection

interpretNodeL nodeRt (L.CloseConnection conn next) = next <$> closeConnection nodeRt conn

interpretNodeL _      _                             = error "not implemented."

-- | Runs node language.
runNodeL :: T.NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
