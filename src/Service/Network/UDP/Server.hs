module Service.Network.UDP.Server (runServer) where

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString

import Control.Monad
import Data.ByteString


-- | Run UDP server.
runServer ::
    PortNumber ->
    (ByteString -> SockAddr -> Socket -> IO ())
    -> IO ()
runServer aPortNumber aPlainHandler = withSocketsDo $ do
    aServerAddr:_ <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
            Nothing (Just $ show aPortNumber)
    aSocket <- socket (addrFamily aServerAddr) Datagram defaultProtocol
    bind aSocket (addrAddress aServerAddr)
    forever $ do
        (aMsg, aHostAddress) <- recvFrom aSocket (1024*100)
        aPlainHandler aMsg aHostAddress aSocket
