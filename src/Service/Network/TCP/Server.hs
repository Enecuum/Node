module Service.Network.TCP.Server (runServer) where

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString

import Control.Monad
import Data.ByteString (ByteString)


-- | Run TCP server.
runServer ::
    PortNumber ->
    (ByteString -> SockAddr -> Socket -> IO ())
    -> IO ()
runServer aPortNumber aPlainHandler = withSocketsDo $ do
    aServerAddr:_ <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
            Nothing (Just $ show aPortNumber)
    aSocket <- socket (addrFamily aServerAddr) Stream defaultProtocol
    setSocketOption aSocket ReuseAddr 1
    bind aSocket (addrAddress aServerAddr)
    listen aSocket 10

    forever $ do
        (conn, peer) <- accept aSocket
        (aMsg, aHostAddress) <- recvFrom conn (1024*100)
        aPlainHandler aMsg aHostAddress conn
