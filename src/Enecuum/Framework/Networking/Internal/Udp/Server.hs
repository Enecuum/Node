module Enecuum.Framework.Networking.Internal.Udp.Server where

import           Control.Concurrent.Async (race)
import           Control.Monad
import           Enecuum.Prelude
import           Network.Socket hiding (recvFrom, sendTo)
import           Network.Socket.ByteString
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.Chan
import           Enecuum.Domain as D
import           Control.Monad.Extra
import           Data.ByteString.Lazy as B (toStrict, fromStrict)


runUDPServer :: TChan ServerComand -> PortNumber -> (LByteString -> (TChan D.SendMsg) -> SockAddr -> IO ()) -> IO ()
runUDPServer chan port handler = bracket (listenUDP port) close $ \sock -> do
    respChan <- atomically $ newTChan
    let 
        sendMsg :: IO ()
        sendMsg = forever $ do
            SendMsg reciver msg <- atomically $ readTChan respChan
            tryMR (sendTo sock (B.toStrict msg) reciver) (\_ -> pure ()) 

        talk :: IO ()
        talk = forever $ tryMR (recvFrom sock (1024)) $
            \(msg, addr) -> handler (B.fromStrict msg) respChan addr

    finally (serv chan sendMsg talk) (close sock)

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
