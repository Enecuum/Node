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
analyzeReadingResult _ _  (Left err) = do
    trace_ $ "[readingWorker] exc in receiving data" <> show err
    pure RFinish

analyzeReadingResult _ _ (Right msg) | null msg  = do
    trace_ "[readingWorker] empty data got"
    pure RFinish

analyzeReadingResult handlers tcpCon (Right msg) | otherwise = do
    trace_ "[readingWorker] message read"
    case decode msg of
        Just (D.NetworkMsg tag val) -> do
            trace_ "[readingWorker] calling handler"
            whenJust (tag `M.lookup` handlers) $ \handler -> handler val tcpCon
            trace_ "[readingWorker] done calling handler"
        Nothing  ->
            trace_ "[readingWorker] decode failed"
    pure RContinue

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: Conn.Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO ()
readingWorker handlers !tcpCon !sock = do
        eRead <- try $ S.recv sock $ toEnum D.packetSize
        readerAct <- analyzeReadingResult handlers tcpCon eRead
        case readerAct of
            RContinue -> do
                trace_ "[readingWorker] continue reading"
                readingWorker handlers tcpCon sock
            RFinish   ->
                trace_ "[readingWorker] finishing"

data AcceptingResult = LLooping | LAccepting (Either SomeException ())
data AcceptorAction  = LContinue | LFinish

acceptingTimeout = 1000 * 1000

acceptConnects :: Socket -> (Socket -> IO ()) -> IO ()
acceptConnects listenSock handler = do
    (connSock, addr) <- accept listenSock
    trace_ $ "[acceptingWorker] accepted some connect, forking handler: " <> show addr
    void $ forkIO $ handler connSock

analyzeAcceptingResult
    :: AcceptingResult
    -> IO AcceptorAction
analyzeAcceptingResult LLooping =
    -- trace_ "[acceptingWorker] looping got after timeout"
    pure LContinue

analyzeAcceptingResult (LAccepting (Left err)) = do
    trace_ $ "[acceptingWorker] exc while accepting connections: " <> show err
    pure LFinish

analyzeAcceptingResult (LAccepting (Right ())) = do
    trace_ "[acceptingWorker] succesfully accepted some connection."
    pure LContinue

acceptingWorker listenSock handler closeSignal = do
    -- trace_ "[acceptingWorker] checking close signal"
    needClose <- readTVarIO closeSignal
    when needClose $ trace_ "[acceptingWorker] need close"
    unless needClose $ do
        let loopDelay = threadDelay acceptingTimeout >> pure LLooping
        let tryAccepting = try $ acceptConnects listenSock handler
        let accepting = LAccepting <$> tryAccepting

        -- trace_ "[acceptingWorker] accepting connections"
        acceptOrLoop <- race loopDelay accepting
        let acceptOrLoopResult = either id id acceptOrLoop

        -- trace_ "[acceptingWorker] analyzing of accepting result"
        loopAct <- analyzeAcceptingResult acceptOrLoopResult
        case loopAct of
            LContinue -> do
                -- trace_ "[acceptingWorker] continue accepting"
                acceptingWorker listenSock handler closeSignal
            LFinish   -> trace_ "[acceptingWorker] finishing"

acceptingHandler registerConnection handlers connSock = do
    trace_ "[acceptingHandler] start, getting network things"
    addr     <- getAdress connSock
    sockPort <- S.socketPort connSock

    let tcpCon = D.Connection $ D.Address addr sockPort
    
    trace_ "[acceptingHandler] starting reading worker"
    let worker = readingWorker handlers tcpCon connSock
                `finally` (trace "[openConnect] readerWorker finished, closing sock" $ manualCloseSock connSock)

    readerId <- forkIO worker

    trace_ $ "[acceptingHandler] registering conn: " <> show addr
    sockVar  <- newTMVarIO connSock
    registered <- registerConnection tcpCon $ D.TcpConnectionVar sockVar readerId

    when registered $
        trace_ "[acceptingHandler] connection registered."

    unless registered (do
        trace_ "[acceptingHandler] closing connSock: connection not registered"
        manualCloseConnection' connSock readerId
        )

instance Conn.NetworkConnection D.Tcp where
    startServer port handlers registerConnection = do
        trace_ "[startServer] start"

        awCloseSignal <- newTVarIO False
        awClosedSignal <- newTMVarIO ()

        let listenF = trace ("[startServer] listenOn " <> show port)
                $ listenOn (PortNumber port)

        eListenSock <- try listenF
        case eListenSock of
            Left (err :: SomeException) -> trace_ $ "[startServer] listenOn failed: " <> show err
            Right listenSock -> do
              trace_ "[startServer] forking accepting worker"
              let handler = acceptingHandler registerConnection handlers
              let worker = acceptingWorker listenSock handler awCloseSignal
                    `finally` (do
                              trace "[startServer] closing listenSock: accepting worker finished" $ S.close listenSock
                              atomically $ putTMVar awClosedSignal ()
                        )
              void $ forkIO worker

        -- Why the openServer function returns valid result when listening failed?
        trace_ "[startServer] done"
        pure $ Conn.ServerHandle awCloseSignal awClosedSignal

    openConnect addr@(D.Address host port) handlers = do
        trace_ "[openConnect] start"

        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- trace "[openConnect] creating sock" $ S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        ok <- trace "[openConnect] connecting to sock" $ try $ S.connect sock $ S.addrAddress address
        r <- case ok of
            Left (err :: SomeException) -> do
                trace ("[openConnect] exc in connect, closing sock: " <> show err) $ S.close sock
                pure Nothing
            Right _ -> do

                sockVar      <- newTMVarIO sock

                let worker = trace "[openConnect] starting read worker" $
                        readingWorker handlers (D.Connection addr) sock
                            `finally` (trace "[openConnect] readerWorker finished, closing sock" $ manualCloseSock sock)
                readerId <- forkIO worker

                pure $ Just $ D.TcpConnectionVar sockVar readerId
        trace_ "[openConnect] done"
        pure r

    close = manualCloseConnection

    send connVar@(D.TcpConnectionVar sockVar readerId) msg
        | length msg > D.packetSize = trace "[send] Too big message" $ pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- trace "[send] Taking sock" $ atomically $ takeTMVar sockVar
            err  <- trace "[send] Sending data" $ try $ S.sendAll sock msg
            res <- case err of
                Right _                     -> pure $ Right ()
                Left  (err :: SomeException)  -> do
                    trace ("[send] exc got, closing " <> show err) $ manualCloseConnection' sock readerId
                    pure $ Left D.ConnectionClosed
            trace "[send] Releasing sock" $ atomically (putTMVar sockVar sock)
            pure res

manualCloseConnection connVar@(D.TcpConnectionVar sockVar readerId) = do
    sock <- trace "[manualCloseConnection] Taking sock" $ atomically $ takeTMVar sockVar
    manualCloseConnection' sock readerId
    trace "[manualCloseConnection] Releasing sock" $ atomically (putTMVar sockVar sock)

manualCloseConnection' sock readerId = do
    trace "[manualCloseConnection] killing reader thread" $ killThread readerId
    manualCloseSock sock
    trace_ "[manualCloseConnection] done"

manualCloseSock sock = do
    trace_ "[manualCloseSock] closing sock"
    eRes <- try $ S.close sock
    whenLeft eRes $ \(err :: SomeException) -> trace_ $ "[manualCloseSock] exc got in closing sock: " <> show err

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket
