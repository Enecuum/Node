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
runServer chan port handler = void $ race 
    (void $ atomically $ readTChan chan) $ do
        sock <- listenOn $ PortNumber port
        finally (forever $ do
            (conn, _) <- accept sock
            void $ forkFinally
                (handler conn)
                (\_ -> close conn))
            
            (close sock)

listenPort :: Show a => a -> IO Socket
listenPort port = do
    serveraddr <- head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing 
        (Just $ show port)
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 5
    return sock