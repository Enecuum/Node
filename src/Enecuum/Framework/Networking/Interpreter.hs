module Enecuum.Framework.Networking.Interpreter where

import           Enecuum.Prelude

import           Data.Aeson                                           as A
import qualified Data.Map                                             as M
import qualified Data.Text                                            as T

import qualified Enecuum.Core.RLens                                   as RLens
import qualified Enecuum.Core.Runtime                                 as R
import qualified Enecuum.Domain                                       as D
import           Enecuum.Framework.Networking.Internal.Client
import qualified Enecuum.Framework.Networking.Internal.Connection     as Conn
import qualified Enecuum.Framework.Networking.Internal.Tcp.Connection as Tcp ()
import qualified Enecuum.Framework.Networking.Internal.Udp.Connection as Udp
import qualified Enecuum.Framework.RLens                              as RLens
import           Enecuum.Framework.Runtime
import qualified Enecuum.Language                                     as L
import qualified Network.Socket                                       as S hiding (recv, send)
import qualified Network.Socket.ByteString.Lazy                       as S
import           Enecuum.Framework.Networking.Internal.Datagram


deleteConnection :: (Ord k,
                    RLens.HasTcpConnects s (TMVar (Map k a)), MonadIO m) =>
                    s -> k -> m ()
deleteConnection nodeRt conn = do
    connects <- atomically $ takeTMVar $ nodeRt ^. RLens.tcpConnects
    let newConnects = M.delete conn connects
    atomically $ putTMVar (nodeRt ^. RLens.tcpConnects) newConnects

-- | Interpret NetworkingL language.
interpretNetworkingL :: NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL _ (L.SendRpcRequest (D.Address host port) request next) =
    next <$> catchAny (do
        -- create a connection
        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        finally (do
            S.connect sock $ S.addrAddress address
            -- send a message
            sendDatagram sock $ A.encode request
            -- read the answer
            msg <- receiveDatagram sock
            -- return the result
            pure $ transformEither T.pack id $ A.eitherDecode msg
            -- close the connection
            ) (S.close sock)
    ) (pure . Left . show)

interpretNetworkingL nodeRt (L.SendTcpMsgByConnection conn msg next) = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    m <- atomically $ readTMVar $ nodeRt ^. RLens.tcpConnects
    case conn `M.lookup` m of
        Just nativeConn -> do
            res <- Conn.send logger nativeConn msg
            when (isLeft res) $ deleteConnection nodeRt conn
            pure $ next res
        Nothing  -> do
            deleteConnection nodeRt conn
            pure $ next $ Left $ D.ConnectionClosed "Connection is dead."

interpretNetworkingL nodeRt (L.SendUdpMsgByConnection conn msg next) = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    m <- atomically $ readTMVar $ nodeRt ^. RLens.udpConnects
    case conn `M.lookup` m of
        Just nativeConn -> next <$> Conn.send logger nativeConn msg
        Nothing  -> do
            connects <- atomically $ takeTMVar $ nodeRt ^. RLens.udpConnects
            let newConnects = M.delete conn connects
            atomically $ putTMVar (nodeRt ^. RLens.udpConnects) newConnects
            pure $ next $ Left $ D.ConnectionClosed "Connection is dead."

interpretNetworkingL _ (L.SendUdpMsgByAddress adr msg next) =
    next <$> Udp.sendUdpMsg adr msg

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left  a) = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingL nodeRt)
