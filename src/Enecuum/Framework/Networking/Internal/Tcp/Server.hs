module Enecuum.Framework.Networking.Internal.Tcp.Server (
      runTCPServer
    , ServerComand(..)
    )  where

import           Control.Concurrent (forkFinally)
import           Control.Concurrent.Async (race)
import           Control.Monad
import           Enecuum.Prelude
import           Network.Socket hiding (recvFrom)
-- import           Network.Socket.ByteString
import           Control.Concurrent.STM.TChan
-- import           Control.Concurrent.Chan
import           Network (PortID (..), listenOn)
import           Enecuum.Domain as D

runTCPServer :: TChan D.ServerComand -> PortNumber -> (Socket -> IO ()) -> IO ()
runTCPServer chan port handler =
    bracket ((listenOn . PortNumber) port) close $ \listenSock ->
        finally (serv chan (acceptConnects listenSock handler)) (close listenSock)

serv :: TChan a -> IO b -> IO ()
serv chan f = void $ race (void $ atomically $ readTChan chan) f

acceptConnects :: forall a b . Socket -> (Socket -> IO a) -> IO b
acceptConnects listenSock handler = forever $ do
    (connSock, _) <- accept listenSock
    void $ forkFinally (handler connSock) (\_ -> close connSock)
