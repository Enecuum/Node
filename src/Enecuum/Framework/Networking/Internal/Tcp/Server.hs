module Enecuum.Framework.Networking.Internal.Tcp.Server (
      runTCPServer
    )  where

import           Control.Concurrent (forkFinally)
import           Control.Monad
import           Enecuum.Prelude
import           Network.Socket hiding (recvFrom)
import           Network (PortID (..), listenOn)
import qualified Enecuum.Core.Runtime      as R
import qualified Enecuum.Framework.Runtime as R

runTCPServer :: R.RuntimeLogger -> PortNumber -> (Socket -> IO ()) -> IO (Maybe R.ServerHandle)
runTCPServer logger port handler = do
    eListenSock <- try $ listenOn (PortNumber port)
    case eListenSock of
        Left (err :: SomeException) -> R.logError' logger (show err) >> pure Nothing
        Right listenSock -> do
            acceptWorkerId <- forkIO $ acceptConnects listenSock handler
            listenSockVar  <- newTMVarIO listenSock
            pure $ Just $ R.ServerHandle listenSockVar acceptWorkerId

acceptConnects :: forall a b . Socket -> (Socket -> IO a) -> IO b
acceptConnects listenSock handler = forever $ do
    (connSock, _) <- accept listenSock
    void $ forkFinally (handler connSock) (\_ -> close connSock)
