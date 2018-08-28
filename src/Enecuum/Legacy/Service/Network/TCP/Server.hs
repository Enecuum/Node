module Service.Network.TCP.Server (runServer) where

import Network.Socket
import Control.Monad
import Control.Concurrent (forkFinally)
import Network (listenOn, PortID(..))


-- | Run TCP server.
runServer :: PortNumber -> (Socket -> IO()) -> IO ()
runServer aPortNumber aPlainHandler = withSocketsDo $ do
    sock <- listenOn $ PortNumber aPortNumber
    forever $ do
        (conn, _) <- accept sock
        void $ forkFinally (aPlainHandler conn) (\_ -> close conn)
