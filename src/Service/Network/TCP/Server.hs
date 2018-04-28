module Service.Network.TCP.Server (runServer) where

import Network (listenOn, PortID(..))
import Network.Socket hiding (recvFrom)  
import Network.Socket.ByteString (recvFrom)

import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)


-- | Run TCP server.
runServer ::
    PortNumber ->
    (ByteString -> SockAddr -> Socket -> IO ())
    -> IO ()
runServer aPortNumber aPlainHandler = do
    sock <- listenOn $ PortNumber aPortNumber
    loop sock

  where loop :: Socket -> IO()
        loop sock = do
          (conn, _) <- accept sock

          _ <- forkIO $ do
            (aMsg, aHostAddress) <- recvFrom conn (1024*100)
            aPlainHandler aMsg aHostAddress conn
            close conn
          loop sock
