module Enecuum.Framework.Networking.Internal.TCP.Server (runServer, ServerComand (..)) where

import           Control.Concurrent (forkFinally)
import           Control.Concurrent.Async (race)
import           Control.Monad
import           Enecuum.Prelude
import           Network            (PortID (..), listenOn)
import           Network.Socket
import           Control.Concurrent.STM.TChan

data ServerComand = StopServer

-- | Run TCP server.
runServer :: TChan ServerComand -> PortNumber -> (Socket -> IO()) -> IO ()
runServer chan aPortNumber aPlainHandler = void $ race 
    (void $ atomically $ readTChan chan) $ do
        sock <- listenOn $ PortNumber aPortNumber
        forkFinally (forever $ do
            (conn, _) <- accept sock
            void $ forkFinally (aPlainHandler conn) (\_ -> close conn))
            (\_ -> close sock)