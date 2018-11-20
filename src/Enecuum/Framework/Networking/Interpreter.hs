module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Map as M

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Networking.Internal.Tcp.Connection as Tcp ()
import qualified Enecuum.Framework.Networking.Internal.Udp.Connection as Udp
import qualified Enecuum.Framework.Networking.Internal.Connection     as Conn
import           Enecuum.Framework.Runtime
import qualified Enecuum.Framework.RLens            as RLens
import qualified Enecuum.Core.RLens                 as RLens
import qualified Enecuum.Core.Runtime               as R
import qualified Network.Socket.ByteString.Lazy     as S
import           Enecuum.Framework.Networking.Internal.Client
import qualified Network.Socket as S hiding (recv, send)


deleteConnection nodeRt conn = do
    connects <- atomically $ takeTMVar $ nodeRt ^. RLens.tcpConnects
    let newConnects = M.delete conn connects 
    atomically $ putTMVar (nodeRt ^. RLens.tcpConnects) newConnects

-- | Interpret NetworkingL language.
interpretNetworkingL :: NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL _ (L.SendRpcRequest addr request next) = do
    var <- newEmptyMVar
    ok  <- try $ runClient S.Stream addr $ \connect -> do
        S.sendAll connect $ A.encode request
        msg <- S.recv connect (1024 * 4)
        putMVar var (transformEither T.pack id $ A.eitherDecode msg)
    case ok of
        Right _                    -> pure ()
        Left  (e :: SomeException) -> putMVar var $ Left $ "Server does not exist: " +|| e ||+ " " +|| addr ||+ "."       
    res <- takeMVar var
    pure $ next res

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
            pure $ next $ Left D.ConnectionClosed

interpretNetworkingL nodeRt (L.SendUdpMsgByConnection conn msg next) = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    m <- atomically $ readTMVar $ nodeRt ^. RLens.udpConnects
    case conn `M.lookup` m of
        Just nativeConn -> next <$> Conn.send logger nativeConn msg
        Nothing  -> do
            connects <- atomically $ takeTMVar $ nodeRt ^. RLens.udpConnects
            let newConnects = M.delete conn connects 
            atomically $ putTMVar (nodeRt ^. RLens.udpConnects) newConnects
            pure $ next $ Left D.ConnectionClosed

interpretNetworkingL _ (L.SendUdpMsgByAddress adr msg next) =
    next <$> Udp.sendUdpMsg adr msg

interpretNetworkingL _ _ = error "interpretNetworkingL EvalNetwork not implemented."

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left  a) = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nodeRt = foldFree (interpretNetworkingL nodeRt)
