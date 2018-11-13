module Enecuum.Framework.Networking.Internal.Connection where

import           Control.Concurrent (killThread)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TChan
import           Data.Aeson
import           Enecuum.Prelude
import qualified Enecuum.Framework.Domain.Networking as D
import qualified Network.Socket                      as S hiding (recv)
import qualified Data.IP as IP
import           Control.Concurrent.STM.TChan (TChan)

-- TODO: get rid of it
data ServerComand = StopServer

type Handler protocol  = Value -> D.Connection protocol -> IO ()
type Handlers protocol = Map Text (Handler protocol)
data ServerHandle
      = ServerHandle (TMVar S.Socket) ThreadId

      -- TODO: get rid of this
      | OldServerHandle (TChan ServerComand)

stopServer (OldServerHandle chan) =
    atomically $ writeTChan chan StopServer

stopServer (ServerHandle var threadId) = do
    sock <- atomically $ readTMVar var
    manualCloseConnection' sock threadId

-- TODO: this is no longer needed, remove
type CloseSignal = TVar Bool

class AsNativeConnection a where
    data family NativeConnection a
    getConnection :: NativeConnection a -> D.Connection a
    getSocketVar  :: NativeConnection a -> TMVar S.Socket
    getReaderId   :: NativeConnection a -> ThreadId

instance AsNativeConnection D.Tcp where
    data NativeConnection D.Tcp
        = TcpConnection (TMVar S.Socket) ThreadId (D.Connection D.Tcp)
    
    getConnection (TcpConnection _       _        conn) = conn
    getSocketVar  (TcpConnection sockVar _        _   ) = sockVar
    getReaderId   (TcpConnection _       readerId _   ) = readerId

-- TODO: Why Client and Server connection are different types?
instance AsNativeConnection D.Udp where
    data NativeConnection D.Udp
        = ServerUdpConnection S.SockAddr  (TMVar S.Socket) (D.Connection D.Udp)
        | ClientUdpConnection ThreadId    (TMVar S.Socket) (D.Connection D.Udp)

    -- TODO: implement
    getConnection (ClientUdpConnection _ _ conn) = conn
    getConnection (ServerUdpConnection _ _ conn) = conn
    
    getSocketVar (ClientUdpConnection _ sockVar _) = sockVar
    getSocketVar (ServerUdpConnection _ sockVar _) = sockVar
    
    getReaderId (ClientUdpConnection readerId _ _) = readerId
    getReaderId _ = error "getReaderId for Udp server native connection not implemented."

data ConnectionRegister protocol = ConnectionRegister
    { addConnection
        :: AsNativeConnection protocol
        => NativeConnection protocol
        -> IO ()
    , removeConnection
        :: AsNativeConnection protocol
        => D.Connection protocol
        -> IO ()
    }

class AsNativeConnection protocol => NetworkConnection protocol where
    startServer :: S.PortNumber -> Handlers protocol -> ConnectionRegister protocol -> IO (Maybe ServerHandle)
    open  :: D.Address -> Handlers protocol -> IO (Maybe (NativeConnection protocol))
    close :: NativeConnection protocol -> IO ()
    send  :: NativeConnection protocol -> LByteString -> IO (Either D.NetworkError ())


manualCloseConnection
    :: AsNativeConnection protocol
    => NativeConnection protocol
    -> IO ()
manualCloseConnection nativeConn = do
    let conn     = getConnection nativeConn
    let sockVar  = getSocketVar nativeConn
    let readerId = getReaderId nativeConn
    sock <- trace ("[manualCloseConnection] " <> show conn <> " Taking sock") $ atomically $ takeTMVar sockVar
    manualCloseConnection' sock readerId
    trace ("[manualCloseConnection] " <> show conn <> " Releasing sock") $ atomically (putTMVar sockVar sock)

manualCloseConnection' :: S.Socket -> ThreadId -> IO ()
manualCloseConnection' sock readerId = do
    trace "[manualCloseConnection] killing thread" $ killThread readerId
    manualCloseSock sock
    trace_ "[manualCloseConnection] done"

manualCloseSock :: S.Socket -> IO ()
manualCloseSock sock = do
    trace_ "[manualCloseSock] closing sock"
    eRes <- try $ S.close sock
    whenLeft eRes $ \(err :: SomeException) -> trace_ $ "[manualCloseSock] exc got in closing sock: " <> show err


showSockAddr :: (Semigroup s, IsString s) => S.SockAddr -> s
showSockAddr (S.SockAddrInet port host)       = show (IP.fromHostAddress  host ) <> ":" <> show port
showSockAddr (S.SockAddrInet6 port _ host6 _) = show (IP.fromHostAddress6 host6) <> ":" <> show port
showSockAddr (S.SockAddrUnix str)             = show str
showSockAddr (S.SockAddrCan sa)               = show $ "SockAddrCan: " <> show sa

unsafeFromSockAddr :: S.SockAddr -> D.Address
unsafeFromSockAddr (S.SockAddrInet port host)       = D.Address (show $ IP.fromHostAddress host)   port
unsafeFromSockAddr (S.SockAddrInet6 port _ host6 _) = D.Address (show $ IP.fromHostAddress6 host6) port
unsafeFromSockAddr (S.SockAddrUnix str)             = D.unsafeParseAddress str
unsafeFromSockAddr sa                               = error $ "fromSockAddr not supported for: " <> show sa

isSocketClosed :: S.Socket -> IO Bool
isSocketClosed (S.MkSocket _ _ _ _ mStat) = do
    status <- readMVar mStat
    pure $ status == S.Closed