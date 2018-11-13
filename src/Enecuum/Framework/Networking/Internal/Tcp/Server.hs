module Enecuum.Framework.Networking.Internal.Tcp.Server (
      runTCPServer
    )  where

import           Control.Concurrent (forkFinally)
import           Control.Monad
import           Enecuum.Prelude
import           Network.Socket hiding (recvFrom)
import           Network (PortID (..), listenOn)
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn

-- TODO: this is wrong design, get rid of it
runTCPServer :: PortNumber -> (Socket -> IO ()) -> IO (Maybe Conn.ServerHandle)
runTCPServer port handler = do
    eListenSock <- try $ listenOn (PortNumber port)
    case eListenSock of
        Left (err :: SomeException) -> pure Nothing
        Right listenSock -> do
            acceptWorkerId <- forkIO $ acceptConnects listenSock handler
            listenSockVar  <- newTMVarIO listenSock
            pure $ Just $ Conn.ServerHandle listenSockVar acceptWorkerId

acceptConnects :: forall a b . Socket -> (Socket -> IO a) -> IO b
acceptConnects listenSock handler = forever $ do
    (connSock, _) <- accept listenSock
    void $ forkFinally (handler connSock) (\_ -> close connSock)
