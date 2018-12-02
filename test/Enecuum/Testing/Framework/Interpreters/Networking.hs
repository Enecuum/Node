module Enecuum.Testing.Framework.Interpreters.Networking where

import           Enecuum.Prelude

import qualified Data.Map                                                as Map

import qualified Enecuum.Domain                                          as D
import qualified Enecuum.Framework.Lens                                  as Lens
import qualified Enecuum.Language                                        as L

import qualified Enecuum.Testing.Core.Interpreters                       as Impl
import           Enecuum.Testing.Framework.Internal.TcpLikeServerBinding (closeConnection)
import qualified Enecuum.Testing.RLens                                   as RLens
import           Enecuum.Testing.TestRuntime                             (controlRequest)
import qualified Enecuum.Testing.Types                                   as T

-- | Relay request from this node to the network environment for target server node.
relayRequest' :: T.NodeRuntime -> D.Address -> D.RpcRequest -> IO (Either Text D.RpcResponse)
relayRequest' nodeRt to req = do
    atomically $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request) $ T.RelayRpcReq (nodeRt ^. RLens.address)
                                                                                           to
                                                                                           req
    controlResponse <- atomically $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
    case controlResponse of
        T.AsRpcResp   rpcResponse -> pure $ Right rpcResponse
        T.AsErrorResp err         -> pure $ Left err
        _                         -> error "Invalid network control result."

-- | Send message to the connection.
sendMessageToConnection :: T.NodeRuntime -> D.Connection D.Tcp  -> D.RawData -> IO (Either Text ())
sendMessageToConnection nodeRt connection@(D.Connection (D.BoundAddress address) _) msg = do
    connections <- atomically $ readTMVar $ nodeRt ^. RLens.connections
    -- Checking is connection alive.
    case Map.lookup address connections of
        Nothing -> do
            -- Dead connection, should remove it
            closeConnection nodeRt connection
            pure $ Left $ "Connection died, connection worker killed: " +|| connection ||+ "."
        Just nodeConnection -> do
            controlResp <- controlRequest (nodeConnection ^. RLens.bindedServer . RLens.handle . RLens.control)
                $ T.MessageReq msg
            case controlResp of
                T.AsSuccessResp -> pure $ Right ()
                _               -> pure $ Left "Unknown control response."



-- | Interpret NetworkingL language.
interpretNetworkingF :: T.NodeRuntime -> L.NetworkingF a -> IO a

interpretNetworkingF nodeRt (L.SendRpcRequest toAddr req next) = next <$> relayRequest' nodeRt toAddr req

interpretNetworkingF nodeRt (L.SendTcpMsgByConnection conn msg next) = do
    void $ sendMessageToConnection nodeRt conn msg
    pure $ next $ Right ()

interpretNetworkingF _ L.SendUdpMsgByConnection{} = error "SendUdpMsgByConnection not implemented"
interpretNetworkingF _ L.SendUdpMsgByAddress{}    = error "SendUdpMsgByAddress not implemented"


-- | Runs networking language.
runNetworkingL :: T.NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingF nodeRt)
