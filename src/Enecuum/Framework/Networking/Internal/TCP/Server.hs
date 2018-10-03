{-# LANGUAGE Strict #-}

module Enecuum.Framework.Networking.Internal.TCP.Server (runServer, ServerComand (..)) where

import           Control.Concurrent (forkFinally)
import           Control.Concurrent.Async (race)
import           Network (PortID (..), listenOn)
import           Control.Monad
import           Enecuum.Prelude
import           Network.Socket
import           Control.Concurrent.STM.TChan


data ServerComand = StopServer

-- | Run TCP server.
runServer :: TChan ServerComand -> PortNumber -> (Socket -> IO()) -> IO ()
runServer chan port handler = bracket 
    (listenOn $ PortNumber port)
    close
    (\sock -> finally 
        (void $ race
            (void $ atomically $ readTChan chan)
            (acceptConnects sock handler))
        (close sock))


acceptConnects :: forall a b. Socket -> (Socket -> IO a) -> IO b
acceptConnects sock handler = forever $ do
    (conn, _) <- accept sock
    void $ forkFinally
        (handler conn)
        (\_ -> close conn)
