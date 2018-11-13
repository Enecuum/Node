{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Framework.Networking.Internal.Udp.Connection
    ( sendUdpMsg
    ) where

import           Enecuum.Prelude
import qualified Data.Map as M
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import           Data.Aeson
import           Control.Concurrent.STM.TChan

import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client
import           Enecuum.Framework.Networking.Internal.Udp.Server
import qualified Network.Socket as S hiding (recv, send, sendTo, sendAll, recvFrom)
import qualified Network.Socket.ByteString as S
import           Control.Monad.Extra
import           Data.ByteString.Lazy         as B (fromStrict, toStrict)

sendMsg sendFunc conn = do
    sock <- atomically $ takeTMVar $ Conn.getSocketVar conn
    err <- try $ sendFunc sock
    atomically (putTMVar (Conn.getSocketVar conn) sock)
    case err of
        Right _                     -> pure $ Right ()
        Left  (_ :: SomeException)  -> do
            Conn.close conn
            pure $ Left D.ConnectionClosed

--
-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
data ReaderAction  = RContinue | RFinish deriving Eq

analyzeReadingResult
    :: Conn.Handlers D.Udp
    -> D.Connection D.Udp
    -> Either SomeException LByteString
    -> IO ReaderAction
analyzeReadingResult _ _  (Left err) = pure RFinish
analyzeReadingResult _ _ (Right msg) | null msg  = pure RFinish

analyzeReadingResult handlers conn (Right msg) = do
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (tag `M.lookup` handlers) $ \handler ->
            handler val conn
    pure RContinue

readingWorker :: Conn.Handlers D.Udp -> D.Connection D.Udp -> S.Socket -> IO ()
readingWorker handlers conn sock = do
    isDead <- Conn.isSocketClosed sock
    unless isDead $ do
        eRead <- try $ S.recv sock $ toEnum D.packetSize
        readerAct <- analyzeReadingResult handlers conn (B.fromStrict <$> eRead)
        when (readerAct == RContinue) $ readingWorker handlers conn sock


sendUdpMsg :: D.Address -> LByteString -> IO (Either D.NetworkError ())
sendUdpMsg _    msg | length msg > D.packetSize = pure $ Left D.TooBigMessage
sendUdpMsg addr msg = tryM
    (runClient S.Datagram addr $ \sock -> S.sendAll sock $ B.toStrict msg)
    (pure $ Left D.AddressNotExist)
    (\_ -> pure $ Right ())


serverWorker (Conn.ConnectionRegister addConn _) handlers sockVar = do
    sock <- atomically $ readTMVar sockVar
    forever $ tryMR (S.recvFrom sock D.packetSize) $ \(rawMsg, sockAddr) -> do
        let message = B.fromStrict rawMsg
        let conn    = D.Connection $ D.BoundAddress $ Conn.unsafeFromSockAddr sockAddr
        addConn (Conn.ServerUdpConnection sockAddr sockVar conn)
        runHandler conn handlers message


runHandler :: D.Connection D.Udp -> Conn.Handlers D.Udp -> LByteString -> IO ()
runHandler netConn handlers msg =
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (handlers ^. at tag) $ \handler ->
            handler val netConn

instance Conn.NetworkConnection D.Udp where
    startServer port handlers register = do
        eListenSock <- try $ listenUDP port
        case eListenSock of
            Left (err :: SomeException) -> pure Nothing
            Right listenSock -> do
                listenSockVar  <- newTMVarIO listenSock
                let worker = serverWorker register handlers listenSockVar
                        `finally` Conn.manualCloseSock listenSock
                serverWorkerId <- forkIO worker 
                pure $ Just $ Conn.ServerHandle listenSockVar serverWorkerId

    send _ msg | length msg > D.packetSize = pure $ Left D.TooBigMessage
    send conn@(Conn.ServerUdpConnection sockAddr sockVar _) msg =
        sendMsg (\sock -> S.sendTo sock (B.toStrict msg) sockAddr) conn
    send conn msg = sendMsg (\sock -> S.sendAll sock (B.toStrict msg)) conn
    

    close conn@Conn.ClientUdpConnection{} = Conn.manualCloseConnection conn
    close _  = pure ()

    open addr@(D.Address host port) handlers = do
        address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock       <- S.socket (S.addrFamily address) S.Datagram S.defaultProtocol
        eConnected <- try $ S.connect sock $ S.addrAddress address

        case eConnected of
            Left (err :: SomeException) -> do
                S.close sock
                pure Nothing
            Right () -> do
                boundAddr <- D.BoundAddress . Conn.unsafeFromSockAddr <$> S.getSocketName sock
                let conn = D.Connection boundAddr
                sockVar <- newTMVarIO sock
                let worker = readingWorker handlers conn sock `finally` S.close sock
                readerId <- forkIO worker
                pure $ Just $ Conn.ClientUdpConnection readerId sockVar conn

