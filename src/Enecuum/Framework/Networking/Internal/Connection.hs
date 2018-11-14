{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Framework.Networking.Internal.Connection where

import           Enecuum.Prelude

import qualified Network.Socket                      as S
import qualified Data.IP                             as IP

import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Runtime 
import qualified Enecuum.Core.Runtime                as R


getNewConnectId :: ConnectCounter -> IO D.ConnectId
getNewConnectId counterVar =
    atomicModifyIORef' counterVar (\counter -> (counter + 1, counter + 1))

stopServer :: ServerHandle -> IO ()
stopServer (ServerHandle var threadId) = do
    sock <- atomically $ readTMVar var
    closeConnection' sock threadId

instance AsNativeConnection D.Tcp where
    data NativeConnection D.Tcp
        = TcpConnection (TMVar S.Socket) ThreadId (D.Connection D.Tcp)
    
    getConnection (TcpConnection _       _        conn) = conn
    getSocketVar  (TcpConnection sockVar _        _   ) = sockVar
    getReaderId   (TcpConnection _       readerId _   ) = readerId

instance AsNativeConnection D.Udp where
    data NativeConnection D.Udp
        = ServerUdpConnection S.SockAddr (TMVar S.Socket) (D.Connection D.Udp)
        | ClientUdpConnection ThreadId   (TMVar S.Socket) (D.Connection D.Udp)

    getConnection (ClientUdpConnection _ _ conn) = conn
    getConnection (ServerUdpConnection _ _ conn) = conn
    
    getSocketVar (ClientUdpConnection _ sockVar _) = sockVar
    getSocketVar (ServerUdpConnection _ sockVar _) = sockVar
    
    getReaderId (ClientUdpConnection readerId _ _) = readerId
    getReaderId _ = error "getReaderId for Udp server native connection not supported."

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
    startServer :: R.RuntimeLogger -> ConnectCounter -> S.PortNumber -> Handlers protocol -> ConnectionRegister protocol -> IO (Maybe ServerHandle)
    open        :: R.RuntimeLogger -> ConnectCounter -> D.Address -> Handlers protocol -> IO (Maybe (NativeConnection protocol))
    close       :: R.RuntimeLogger -> NativeConnection protocol -> IO ()
    send        :: R.RuntimeLogger -> NativeConnection protocol -> LByteString -> IO (Either D.NetworkError ())


closeConnection
    :: AsNativeConnection protocol
    => NativeConnection protocol
    -> IO ()
closeConnection nativeConn = do
    let sockVar  = getSocketVar nativeConn
    let readerId = getReaderId nativeConn
    sock <- atomically $ takeTMVar sockVar
    closeConnection' sock readerId
    atomically (putTMVar sockVar sock)

closeConnection' :: S.Socket -> ThreadId -> IO ()
closeConnection' sock readerId = do
    killThread readerId
    closeSocket sock

closeSocket :: S.Socket -> IO ()
closeSocket sock = do
    eRes <- try $ S.close sock
    whenLeft eRes $ \(_ :: SomeException) -> pure ()


showSockAddr :: (Semigroup s, IsString s) => S.SockAddr -> s
showSockAddr (S.SockAddrInet port host)       = show (IP.fromHostAddress  host ) <> ":" <> show port
showSockAddr (S.SockAddrInet6 port _ host6 _) = show (IP.fromHostAddress6 host6) <> ":" <> show port
showSockAddr (S.SockAddrUnix str)             = show str
showSockAddr sa                               = "Unsupported sockAddr" <> show sa

unsafeFromSockAddr :: S.SockAddr -> D.Address
unsafeFromSockAddr (S.SockAddrInet port host)       = D.Address (show $ IP.fromHostAddress host)   port
unsafeFromSockAddr (S.SockAddrInet6 port _ host6 _) = D.Address (show $ IP.fromHostAddress6 host6) port
unsafeFromSockAddr (S.SockAddrUnix str)             = D.unsafeParseAddress str
unsafeFromSockAddr sa                               = error $ "fromSockAddr not supported for: " <> show sa

isSocketClosed :: S.Socket -> IO Bool
isSocketClosed (S.MkSocket _ _ _ _ mStat) = do
    status <- readMVar mStat
    pure $ status == S.Closed