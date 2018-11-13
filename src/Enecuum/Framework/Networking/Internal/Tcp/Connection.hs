{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import           Data.Aeson
import           Control.Concurrent (forkFinally, killThread)
import           Control.Concurrent.Async (race)
import qualified Data.Map as M

import           Network.Socket hiding (recvFrom)
import           Control.Concurrent.STM.TChan
import           Network (PortID (..), listenOn)
import           Enecuum.Domain as D

import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Tcp.Server
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)

data ReaderAction  = RContinue | RFinish deriving Eq

analyzeReadingResult
    :: Conn.Handlers D.Tcp
    -> D.Connection D.Tcp
    -> Either SomeException LByteString
    -> IO ReaderAction
analyzeReadingResult _        _     (Left err)            = pure RFinish
analyzeReadingResult _        _    (Right msg) | null msg = pure RFinish
analyzeReadingResult handlers conn (Right msg) = do
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (tag `M.lookup` handlers) $ \handler ->
            handler val conn
    pure RContinue

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: Conn.Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO ()
readingWorker handlers conn sock = do
    isDead <- Conn.isSocketClosed sock
    unless isDead $ do
        eRead <- try $ S.recv sock $ toEnum D.packetSize
        readerAct <- analyzeReadingResult handlers conn eRead
        when (readerAct == RContinue) $ readingWorker handlers conn sock

data AcceptorAction  = LContinue | LFinish deriving Eq

acceptConnects :: Socket -> ((Socket, D.BoundAddress) -> IO ()) -> IO ()
acceptConnects listenSock handler = do
    (connSock, boundAddr') <- accept listenSock
    let boundAddr = D.BoundAddress $ Conn.unsafeFromSockAddr boundAddr'
    void $ forkIO $ handler (connSock, boundAddr)

analyzeAcceptingResult :: Either SomeException () -> IO AcceptorAction
analyzeAcceptingResult (Left err) = pure LFinish
analyzeAcceptingResult (Right ()) = pure LContinue

acceptingWorker listenSock handler = do
    eRes <- try $ acceptConnects listenSock handler
    loopAct <- analyzeAcceptingResult eRes
    when (loopAct == LContinue) $ acceptingWorker listenSock handler

acceptingHandler connectCounter (Conn.ConnectionRegister addConn removeConn) handlers (connSock, boundAddr) = do
    connectId <- Conn.getNewConnectId connectCounter
    let conn = D.Connection boundAddr connectId
    let worker = readingWorker handlers conn connSock
            `finally` (removeConn conn >> Conn.manualCloseSock connSock)
    readerId <- forkIO worker
    sockVar <- newTMVarIO connSock
    let nativeConn = Conn.TcpConnection sockVar readerId conn
    addConn nativeConn

-- TODO: register / unregister listener?
instance Conn.NetworkConnection D.Tcp where
    startServer connectCounter port handlers register = do
        eListenSock <- try $ listenOn (PortNumber port)
        case eListenSock of
            Left (err :: SomeException) -> pure Nothing
            Right listenSock -> do
                let handler = acceptingHandler connectCounter register handlers
                let worker = acceptingWorker listenSock handler
                                `finally` Conn.manualCloseSock listenSock

                acceptWorkerId <- forkIO worker
                listenSockVar <- newTMVarIO listenSock
                pure $ Just $ Conn.ServerHandle listenSockVar acceptWorkerId

    open counter serverAddr@(D.Address host port) handlers = do
        address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock       <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        eConnected <- try $ S.connect sock $ S.addrAddress address

        case eConnected of
            Left (err :: SomeException) -> do
                S.close sock
                pure Nothing
            Right () -> do
                connectId <- Conn.getNewConnectId counter
                boundAddr <- D.BoundAddress . Conn.unsafeFromSockAddr <$> S.getSocketName sock
                let conn = D.Connection boundAddr connectId
                sockVar <- newTMVarIO sock
                let worker = readingWorker handlers conn sock
                        `finally` Conn.manualCloseSock sock
                readerId <- forkIO worker
                pure $ Just $ Conn.TcpConnection sockVar readerId conn

    close = Conn.manualCloseConnection

    send (Conn.TcpConnection sockVar readerId boundAddr) msg
        | length msg > D.packetSize = pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- atomically $ takeTMVar sockVar
            eRes <- try $ S.sendAll sock msg
            res <- case eRes of
                Right _                     -> pure $ Right ()
                Left  (err :: SomeException)  -> do
                    Conn.manualCloseConnection' sock readerId
                    pure $ Left D.ConnectionClosed
            atomically (putTMVar sockVar sock)
            pure res
