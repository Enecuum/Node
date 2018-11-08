module Enecuum.Framework.Networking.Internal.Udp.Server where

import           Control.Concurrent.Async     (race)
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.Extra
import           Data.ByteString.Lazy         as B (fromStrict, toStrict)
import           Enecuum.Domain               as D
import           Enecuum.Prelude
import           Network.Socket               hiding (recvFrom, sendTo)
import           Network.Socket.ByteString


runUDPServer :: TChan ServerComand -> PortNumber -> (Socket -> SockAddr -> LByteString ->  IO ()) -> IO ()
runUDPServer chan port handler = bracket (listenUDP port) close $ \sock -> do
    let talk :: IO ()
        talk = forever $ tryMR (recvFrom sock D.packetSize) $
            \(msg, addr) -> handler sock addr (B.fromStrict msg) 
    finally (void $ race (void $ atomically $ readTChan chan) talk) (close sock)

serv :: TChan a -> IO b -> IO c -> IO ()
serv chan f g = void $ race (void $ atomically $ readTChan chan) (race f g)

listenUDP :: PortNumber -> IO Socket
listenUDP port = do
    serveraddr:_ <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing
        (Just $ show port)
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    pure sock
