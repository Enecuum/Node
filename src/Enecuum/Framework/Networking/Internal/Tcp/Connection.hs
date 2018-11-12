{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import           Data.Aeson
import           Control.Concurrent (forkFinally, killThread)
import           Control.Concurrent.STM.TChan
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

data ReaderAction  = RContinue | RFinish

analyzeReadingResult
    :: Conn.Handlers D.Tcp
    -> D.Connection D.Tcp
    -> Either SomeException LByteString
    -> IO ReaderAction
analyzeReadingResult _ _  (Left err) = do
    trace_ $ "[readingWorker] exc in receiving data" <> show err
    pure RFinish

analyzeReadingResult _ _ (Right msg) | null msg  = do
    trace_ "[readingWorker] empty data got"
    pure RFinish

analyzeReadingResult handlers conn (Right msg) | otherwise = do
    trace_ "[readingWorker] message read"
    case decode msg of
        Just (D.NetworkMsg tag val) -> do
            trace_ "[readingWorker] calling handler"
            whenJust (tag `M.lookup` handlers) $ \handler -> handler val conn
            trace_ "[readingWorker] done calling handler"
        Nothing  ->
            trace_ "[readingWorker] decode failed"
    pure RContinue

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: Conn.Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO ()
readingWorker handlers !conn !sock = do
    -- TODO: check if closed:
    -- isDead <- D.isClosed conn
    eRead <- try $ S.recv sock $ toEnum D.packetSize
    readerAct <- analyzeReadingResult handlers conn eRead
    case readerAct of
        RContinue -> do
            trace_ "[readingWorker] continue reading"
            readingWorker handlers conn sock
        RFinish   ->
            trace_ "[readingWorker] finishing"

data AcceptorAction  = LContinue | LFinish

acceptConnects :: Socket -> ((Socket, D.BoundAddress) -> IO ()) -> IO ()
acceptConnects listenSock handler = do
    (connSock, boundAddr') <- accept listenSock
    let boundAddr = D.BoundAddress $ Conn.unsafeFromSockAddr boundAddr'
    trace_ $ "[acceptingWorker] accepted some connect. bound addr: " <> show boundAddr <> ". Forking accepting handler."
    void $ forkIO $ handler (connSock, boundAddr)

analyzeAcceptingResult :: Either SomeException () -> IO AcceptorAction
analyzeAcceptingResult (Left err) = do
    trace_ $ "[acceptingWorker] exc while accepting connections: " <> show err
    pure LFinish
analyzeAcceptingResult (Right ()) = do
    trace_ $ "[acceptingWorker] succesfully accepted some connection."
    pure LContinue

acceptingWorker listenSock handler = do
        trace_ "[acceptingWorker] accepting connections"
        eRes <- try $ acceptConnects listenSock handler
        loopAct <- analyzeAcceptingResult eRes
        case loopAct of
            LContinue -> do
                trace_ "[acceptingWorker] continue accepting"
                acceptingWorker listenSock handler
            LFinish   -> trace_ "[acceptingWorker] finishing"

acceptingHandler registerConnection handlers (connSock, boundAddr) = do
    trace_ $ "[acceptingHandler] preparing connection: " <> show boundAddr

    let conn = D.Connection boundAddr
    
    trace_ "[acceptingHandler] starting reading worker"
    let worker = readingWorker handlers conn connSock
                `finally` (trace "[openConnect] readingWorker finished, closing sock" $ Conn.manualCloseSock connSock)

    readerId <- forkIO worker

    trace_ $ "[acceptingHandler] registering conn: " <> show boundAddr
    sockVar    <- newTMVarIO connSock
    registered <- registerConnection conn $ D.TcpConnection sockVar readerId boundAddr

    when registered $
        trace_ "[acceptingHandler] connection registered."

    unless registered $ do
        trace_ "[acceptingHandler] closing connSock: connection not registered"
        Conn.manualCloseConnection' connSock readerId

instance Conn.NetworkConnection D.Tcp where
    startServer port handlers registerConnection = do
        trace_ "[startServer] start"

        let listenF = trace ("[startServer] listenOn " <> show port)
                $ listenOn (PortNumber port)

        eListenSock <- try listenF
        case eListenSock of
            Left (err :: SomeException) -> trace ("[startServer] listenOn failed: " <> show err) $ pure Nothing
            Right listenSock -> do
                trace_ "[startServer] forking accepting worker"
                let handler = acceptingHandler registerConnection handlers
                let worker = acceptingWorker listenSock handler
                                `finally` (trace "[startServer] closing listenSock: accepting worker finished" 
                                                $ Conn.manualCloseSock listenSock)
              
                acceptWorkerId <- forkIO worker
                listenSockVar <- newTMVarIO listenSock
                trace_ "[startServer] successfully started."
                pure $ Just $ Conn.ServerHandle listenSockVar acceptWorkerId

    open serverAddr@(D.Address host port) handlers = do
        trace_ $ "[open] connecting to server: " <> show serverAddr

        address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        trace_ $ "[open] Address info got: " <> show address <> ". Creating socket..."
        sock       <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        trace_ "[open] connecting to sock..."
        eConnected <- try $ S.connect sock $ S.addrAddress address

        mbConnections <- case eConnected of
            Left (err :: SomeException) -> do
                trace ("[open] exc in connect, closing sock: " <> show err) $ S.close sock
                pure Nothing
            Right () -> do
                boundAddr <- D.BoundAddress . Conn.unsafeFromSockAddr <$> S.getSocketName sock
                trace_ $ "[open] Bound address: " <> show boundAddr
                let conn = D.Connection boundAddr
                sockVar <- newTMVarIO sock
                let worker = trace "[open] starting read worker" $
                        readingWorker handlers conn sock
                            `finally` (trace  "[open] readingWorker finished, closing sock" $ Conn.manualCloseSock sock)
                readerId <- forkIO worker
                pure $ Just (conn, D.TcpConnection sockVar readerId boundAddr)
        trace_ "[open] done"

        pure mbConnections

    close conn = do
        trace_ "[close] start"
        Conn.manualCloseConnection conn
        trace_ "[close] done"

    send (D.TcpConnection sockVar readerId boundAddr) msg
        | length msg > D.packetSize = trace ("[send] " <> show boundAddr <> " Too big message") $ pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- trace ("[send] " <> show boundAddr <> " Taking sock") $ atomically $ takeTMVar sockVar
            eRes <- trace ("[send] " <> show boundAddr <> " Sending data") $ try $ S.sendAll sock msg
            res <- case eRes of
                Right _                     -> pure $ Right ()
                Left  (err :: SomeException)  -> do
                    trace ("[send] " <> show boundAddr <> " exc got, closing " <> show err) $ Conn.manualCloseConnection' sock readerId
                    pure $ Left D.ConnectionClosed
            trace ("[send] " <> show boundAddr <> " Releasing sock") $ atomically (putTMVar sockVar sock)
            pure res

getAddress :: S.Socket -> IO D.Host
getAddress socket = D.sockAddrToHost <$> S.getSocketName socket
