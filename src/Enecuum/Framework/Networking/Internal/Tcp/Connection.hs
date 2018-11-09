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

readingTimeout = 1000 * 1000

analyzeReadingResult
    :: Conn.Handlers D.Tcp
    -> D.Connection D.Tcp
    -> Either SomeException LByteString
    -> IO ReaderAction
analyzeReadingResult handlers tcpCon  (Left err) = do
    trace @String ("[readingWorker] exc in receiving data" <> show err) $ pure ""
    pure RFinish

analyzeReadingResult handlers tcpCon (Right msg) | null msg  = do
    trace @String "[readingWorker] empty data got" $ pure ""
    pure RFinish

analyzeReadingResult handlers tcpCon (Right msg) | otherwise = do
    trace @String "[readingWorker] message read" $ pure ()
    case decode msg of
        Just (D.NetworkMsg tag val) -> do
            trace @String "[readingWorker] calling handler" $ pure ""
            whenJust (tag `M.lookup` handlers) $ \handler -> handler val tcpCon
            trace @String "[readingWorker] done calling handler" $ pure ()
        Nothing  -> do
            trace @String "[readingWorker] decode failed" $ pure ()
    pure RContinue

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: Conn.Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO ()
readingWorker handlers !tcpCon !sock = do
        eRead <- try $ S.recv sock $ toEnum D.packetSize
        readerAct <- analyzeReadingResult handlers tcpCon eRead
        case readerAct of
            RContinue -> do
                trace @String "[readingWorker] continue reading" $ pure ()
                readingWorker handlers tcpCon sock
            RFinish   -> do
                trace @String "[readingWorker] finishing" $ pure ()

data AcceptorAction  = LContinue | LFinish

acceptingTimeout = 1000 * 1000

acceptConnects :: Socket -> (Socket -> IO ()) -> IO ()
acceptConnects listenSock handler = do
    (connSock, addr) <- accept listenSock
    trace @String ("[acceptingWorker] accepted some connect, forking handler: " <> show addr) $ pure ()
    void $ forkIO $ handler connSock

analyzeAcceptingResult :: Either SomeException () -> IO AcceptorAction
analyzeAcceptingResult (Left err) = do
    trace @String ("[acceptingWorker] exc while accepting connections: " <> show err) $ pure ""
    pure LFinish
analyzeAcceptingResult (Right ()) = do
    trace @String "[acceptingWorker] succesfully accepted some connection." $ pure ""
    pure LContinue

acceptingWorker listenSock handler = do
        trace @String "[acceptingWorker] accepting connections" $ pure ()
        eRes <- try $ acceptConnects listenSock handler
        loopAct <- analyzeAcceptingResult eRes
        case loopAct of
            LContinue -> do
                trace @String "[acceptingWorker] continue accepting" $ pure ()
                acceptingWorker listenSock handler
            LFinish   -> trace @String "[acceptingWorker] finishing" $ pure ()

acceptingHandler registerConnection handlers connSock = do
    trace @String "[acceptingHandler] start, getting network things" $ pure ()
    addr     <- getAdress connSock
    sockPort <- S.socketPort connSock

    let tcpCon = D.Connection $ D.Address addr sockPort
    
    trace @String "[acceptingHandler] starting reading worker" $ pure ()
    let worker = readingWorker handlers tcpCon connSock
                `finally` (trace @String "[openConnect] readerWorker finished, closing sock" $ Conn.manualCloseSock connSock)

    readerId <- forkIO worker

    trace @String ("[acceptingHandler] registering conn: " <> show addr) $ pure ()
    sockVar  <- newTMVarIO connSock
    registered <- registerConnection tcpCon $ D.TcpConnectionVar sockVar readerId

    when registered $
        trace @String "[acceptingHandler] connection registered." $ pure ()

    unless registered $ (do
        trace @String "[acceptingHandler] closing connSock: connection not registered" $ pure ()
        Conn.manualCloseConnection' connSock readerId
        )

instance Conn.NetworkConnection D.Tcp where
    startServer port handlers registerConnection = do
        trace @String "[startServer] start" $ pure ()

        let listenF = trace @String ("[startServer] listenOn " <> show port)
                $ listenOn (PortNumber port)

        eListenSock <- try listenF
        case eListenSock of
            Left (err :: SomeException) -> trace @String ("[startServer] listenOn failed: " <> show err) $ pure Nothing
            Right listenSock -> do
                trace @String "[startServer] forking accepting worker" $ pure ()
                let handler = acceptingHandler registerConnection handlers
                let worker = acceptingWorker listenSock handler
                                `finally` (trace @String "[startServer] closing listenSock: accepting worker finished" 
                                                $ Conn.manualCloseSock listenSock)
              
                acceptWorkerId <- forkIO worker
                listenSockVar <- newTMVarIO listenSock
                trace @String "[startServer] successfully started." $ pure ()
                pure $ Just $ Conn.ServerHandle listenSockVar acceptWorkerId


    openConnect addr@(D.Address host port) handlers = do
        trace @String "[openConnect] start" $ pure ()

        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- trace @String "[openConnect] creating sock" $ S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        ok <- trace @String "[openConnect] connecting to sock" $ try $ S.connect sock $ S.addrAddress address
        r <- case ok of
            Left (err :: SomeException) -> do
                trace @String ("[openConnect] exc in connect, closing sock: " <> show err) $ S.close sock
                pure Nothing
            Right _ -> do
                sockVar      <- newTMVarIO sock
                let worker = trace @String "[openConnect] starting read worker" $
                        readingWorker handlers (D.Connection addr) sock
                            `finally` (trace @String "[openConnect] readerWorker finished, closing sock" $ Conn.manualCloseSock sock)
                readerId <- forkIO worker

                pure $ Just $ D.TcpConnectionVar sockVar readerId
        trace @String "[openConnect] done" $ pure ()
        pure r

    close = Conn.manualCloseConnection

    send connVar@(D.TcpConnectionVar sockVar readerId) msg
        | length msg > D.packetSize = trace @String "[send] Too big message" $ pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- trace @String "[send] Taking sock" $ atomically $ takeTMVar sockVar
            err  <- trace @String "[send] Sending data" $ try $ S.sendAll sock msg
            res <- case err of
                Right _                     -> pure $ Right ()
                Left  (err :: SomeException)  -> do
                    trace @String ("[send] exc got, closing " <> show err) $ Conn.manualCloseConnection' sock readerId
                    pure $ Left D.ConnectionClosed
            trace @String "[send] Releasing sock" $ atomically (putTMVar sockVar sock)
            pure res

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket
