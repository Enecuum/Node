module Enecuum.Testing.Framework.Interpreters.Node where

import           Enecuum.Prelude


import qualified Enecuum.Domain                                          as D
import qualified Enecuum.Framework.Lens                                  as Lens
import qualified Enecuum.Language                                        as L

import           Enecuum.Core.HGraph.Interpreters.IO                     (runHGraphIO)
import qualified Enecuum.Testing.Core.Interpreters                       as Impl
import           Enecuum.Testing.Framework.Internal.TcpLikeServerBinding (bindServer, closeConnection,
                                                                          registerConnection)
import           Enecuum.Testing.Framework.Internal.TcpLikeServerWorker  (startNodeTcpLikeWorker)
import qualified Enecuum.Testing.Framework.Interpreters.Networking       as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State            as Impl
import qualified Enecuum.Testing.RLens                                   as RLens
import           Enecuum.Testing.TestRuntime                             (controlRequest)
import qualified Enecuum.Testing.Types                                   as T

import qualified Enecuum.Framework.Handler.Network.Interpreter           as Net


-- | Establish connection with the server through test environment.
-- TODO: check if connection exists.
establishConnection :: T.NodeRuntime -> D.Address -> IO (Either Text T.BindedServer)
establishConnection nodeRt toAddress = do
    atomically $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request) $ T.RelayEstablishConnectionReq toAddress
    controlResponse <- atomically $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
    case controlResponse of
        T.AsConnectionAccepted bindedServer -> pure $ Right bindedServer
        T.AsErrorResp          msg          -> pure $ Left $ "Failed to establish connection: " <> msg
        _                                   -> error "Invalid network control result."

-- | Send client connection to the binded server.
sendClientConnection :: T.BindedServer -> T.BindedServer -> IO (Either Text ())
sendClientConnection bindedServer bindedClientsideServer = do
    controlResp <- controlRequest (bindedServer ^. RLens.handle . RLens.control)
        $ T.AcceptBackConnectionReq bindedClientsideServer
    case controlResp of
        T.AsSuccessResp -> pure $ Right ()
        _               -> pure $ Left "Unknown control response."

-- | Interpret NodeL.
interpretNodeF :: T.NodeRuntime -> L.NodeF a -> IO a

interpretNodeF nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> atomically (Impl.runStateL nodeRt statefulAction)

interpretNodeF _      (L.EvalGraphIO gr act next             ) = next <$> runHGraphIO gr act

interpretNodeF nodeRt (L.EvalNetworking networkingAction next) = next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeF nodeRt (L.EvalCoreEffect coreEffect next) =
    next <$> Impl.runCoreEffectL (nodeRt ^. RLens.loggerRuntime) coreEffect

interpretNodeF nodeRt (L.OpenTcpConnection serverAddress handlersF next) = do
  -- Asking the server to accept connection
    eBindedServer <- establishConnection nodeRt serverAddress
    case eBindedServer of
        Left  err          -> error err
        Right bindedServer -> do
            let bindedServerConnection = D.Connection (D.BoundAddress $ bindedServer ^. RLens.address) 0

            -- Collecting hanlders for this connection
            tHandlers <- atomically $ newTVar mempty
            Net.runNetworkHandlerL tHandlers handlersF
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
                    pure $ next $ Just bindedServerConnection

interpretNodeF nodeRt (L.CloseTcpConnection conn next) = next <$> closeConnection nodeRt conn

interpretNodeF _      _                             = error "not implemented."

-- | Runs node language.
runNodeL :: T.NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeF nodeRt)
