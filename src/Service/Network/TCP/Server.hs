module Service.Network.TCP.Server (runServer) where

import Network.Socket
import Network.Socket.ByteString()

import Control.Monad
import Data.ByteString()
import qualified Control.Exception as E (bracket)
import Control.Concurrent (forkFinally)

-- | Run TCP server.

runServer :: PortNumber -> (Socket -> IO()) -> IO ()
runServer aPortNumber aPlainHandler = withSocketsDo $ do
    addr <- resolve aPortNumber
    E.bracket (open addr) close loop
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show port)
        return addr

    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 10
        return sock

    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (aPlainHandler conn) (\_ -> close conn)
{-
    talk conn = forever $ do
        (aMsg, aHostAddress) <- recvFrom conn (1024*100)
        aPlainHandler aMsg aHostAddress conn
-}
