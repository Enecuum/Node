{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import           Data.Aeson
import           Control.Concurrent (forkFinally)
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

data ReadingResult = RLooping | RReading (Either SomeException LByteString)
data ReaderAction  = RContinue | RFinish

readingTimeout = 1000 * 1000


-- runTCPServer :: TChan D.ServerComand -> PortNumber -> (Socket -> IO ()) -> IO ()
-- runTCPServer chan port handler =
--     bracket ((listenOn . PortNumber) port) close $ \listenSock ->
--         finally 
--              (void $ race (void $ atomically $ readTChan chan) 
--                           (acceptConnects listenSock handler)) 
--              (close listenSock)
--
-- serv :: TChan a -> IO b -> IO ()
-- serv chan f = void $ race (void $ atomically $ readTChan chan) f

-- acceptConnects :: forall a b . Socket -> (Socket -> IO a) -> IO b
-- acceptConnects listenSock handler = forever $ do
--     (connSock, _) <- accept listenSock
--     void $ forkFinally (handler connSock) (\_ -> close c

analyzeReadingResult
    :: Conn.Handlers D.Tcp
    -> D.Connection D.Tcp
    -> ReadingResult
    -> IO ReaderAction
analyzeReadingResult handlers tcpCon RLooping = do
    -- trace @String "[readingWorker] looping got after timeout" $ pure ""
    pure RContinue

analyzeReadingResult handlers tcpCon (RReading (Left err)) = do
    trace @String ("[readingWorker] exc in receiving data" <> show err) $ pure ""
    pure RFinish

analyzeReadingResult handlers tcpCon (RReading (Right msg)) | null msg  = do
    trace @String "[readingWorker] empty data got" $ pure ""
    pure RFinish

analyzeReadingResult handlers tcpCon (RReading (Right msg)) | otherwise = do
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
readingWorker :: TVar Bool -> Conn.Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO ()
readingWorker closeSignal handlers !tcpCon !sock = do
    -- trace @String "[readingWorker] checking close signal" $ pure ()
    needClose <- readTVarIO closeSignal
    when needClose $ trace @String "[readingWorker] need close" $ pure ()
    unless needClose $ do
        let loopDelay  = threadDelay readingTimeout >> pure RLooping
        let tryReading :: IO (Either SomeException LByteString) = try $ S.recv sock $ toEnum D.packetSize
        let reading :: IO ReadingResult = RReading <$> tryReading

        readOrLoopResult :: (Either ReadingResult ReadingResult) <- trace @String "[readingWorker] receiving data" $ race loopDelay reading
        let readingResult = either id id readOrLoopResult

        -- trace @String "[readingWorker] analyzing reading result" $ pure ()
        readerAct <- analyzeReadingResult handlers tcpCon readingResult
        case readerAct of
            RContinue -> do
                -- trace @String "[readingWorker] continue reading"
                readingWorker closeSignal handlers tcpCon sock
            RFinish   -> trace @String "[readingWorker] finishing" $ pure ()

makeTcpCon :: S.Socket -> IO (D.ConnectionVar D.Tcp)
makeTcpCon sock = do
    closeSignal  <- newTVarIO False
    closedSignal <- newTMVarIO ()
    sockVar      <- newTMVarIO sock
    pure $ D.TcpConnectionVar closeSignal closedSignal sockVar

data AcceptingResult = LLooping | LAccepting (Either SomeException ())
data AcceptorAction  = LContinue | LFinish

acceptingTimeout = 1000 * 1000

acceptConnects :: Socket -> (Socket -> IO ()) -> IO ()
acceptConnects listenSock handler = do
    (connSock, addr) <- accept listenSock
    trace @String ("[acceptingWorker] accepted some connect, forking handler: " <> show addr) $ pure ()
    void $ forkIO $ handler connSock

analyzeAcceptingResult
    :: AcceptingResult
    -> IO AcceptorAction
analyzeAcceptingResult LLooping = do
    -- trace @String "[acceptingWorker] looping got after timeout" $ pure ""
    pure LContinue

analyzeAcceptingResult (LAccepting (Left err)) = do
    trace @String ("[acceptingWorker] exc while accepting connections: " <> show err) $ pure ""
    pure LFinish

analyzeAcceptingResult (LAccepting (Right ())) = do
    trace @String "[acceptingWorker] succesfully accepted some connection." $ pure ""
    pure LContinue

acceptingWorker listenSock handler closeSignal = do
    -- trace @String "[acceptingWorker] checking close signal" $ pure ()
    needClose <- readTVarIO closeSignal
    when needClose $ trace @String "[acceptingWorker] need close" $ pure ()
    unless needClose $ do
        let loopDelay = threadDelay acceptingTimeout >> pure LLooping
        let tryAccepting = try $ acceptConnects listenSock handler
        let accepting = LAccepting <$> tryAccepting

        -- trace @String "[acceptingWorker] accepting connections" $ pure ()
        acceptOrLoop <- race loopDelay accepting
        let acceptOrLoopResult = either id id acceptOrLoop

        -- trace @String "[acceptingWorker] analyzing of accepting result" $ pure ()
        loopAct <- analyzeAcceptingResult acceptOrLoopResult
        case loopAct of
            LContinue -> do
                -- trace @String "[acceptingWorker] continue accepting" $ pure ()
                acceptingWorker listenSock handler closeSignal
            LFinish   -> trace @String "[acceptingWorker] finishing" $ pure ()

acceptingHandler registerConnection handlers connSock = do
    trace @String "[acceptingHandler] start, getting network things" $ pure ()
    addr     <- getAdress connSock
    sockPort <- S.socketPort connSock
    tcpConVar@(D.TcpConnectionVar closeSignal closedSignal _) <- makeTcpCon connSock
    let tcpCon = D.Connection $ D.Address addr sockPort
    trace @String ("[acceptingHandler] registering conn: " <> show addr) $ pure ()
    ok <- registerConnection tcpCon tcpConVar
    when ok $ do
        trace @String "[acceptingHandler] starting reading worker" $ pure ()
        let worker = readingWorker closeSignal handlers tcpCon connSock
              `finally` (do
                  trace @String "[acceptingHandler] closing connSock: worker finished"
                      $ S.close connSock
                  atomically $ putTMVar closedSignal ()
                  )
        void $ forkIO worker
    unless ok $ (do
        trace @String "[acceptingHandler] closing connSock: connection not registered" $ pure ()
        S.close connSock
        )

instance Conn.NetworkConnection D.Tcp where
    startServer port handlers registerConnection = do
        trace @String "[startServer] start" $ pure ()

        awCloseSignal <- newTVarIO False
        awClosedSignal <- newTMVarIO ()

        let listenF = trace @String ("[startServer] listenOn " <> show port)
                $ listenOn (PortNumber port)

        eListenSock <- try listenF
        case eListenSock of
            Left (err :: SomeException) -> trace @String ("[startServer] listenOn failed: " <> show err) $ pure ()
            Right listenSock -> do
              trace @String "[startServer] forking accepting worker" $ pure ()
              let handler = acceptingHandler registerConnection handlers
              let worker = acceptingWorker listenSock handler awCloseSignal
                    `finally` (do
                              trace @String "[startServer] closing listenSock: accepting worker finished" $ S.close listenSock
                              atomically $ putTMVar awClosedSignal ()
                        )
              void $ forkIO worker

        -- Why the openServer function returns valid result when listening failed?
        trace @String "[startServer] done" $ pure ()
        pure $ Conn.ServerHandle awCloseSignal awClosedSignal

    openConnect addr@(D.Address host port) handlers = do
        trace @String "[openConnect] start" $ pure ()

        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- trace @String "[openConnect] creating sock" $ S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        ok <- trace @String "[openConnect] connecting to sock" $ try $ S.connect sock $ S.addrAddress address
        r <- case ok of
            Left (_ :: SomeException) -> do
                trace @String "[openConnect] exc in connect. Closing sock" $ S.close sock
                pure Nothing
            Right _ -> do
                tcpConVar@(D.TcpConnectionVar closeSignal closedSignal _) <- makeTcpCon sock
                let worker = trace @String "[openConnect] starting read worker" $
                        readingWorker closeSignal handlers (D.Connection addr) sock
                        `finally` (do
                            trace @String "[openConnect] closing sock: worker finished" $ S.close sock
                            atomically $ putTMVar closedSignal ()
                            )

                void $ forkIO worker
                pure $ Just tcpConVar
        trace @String "[openConnect] done" $ pure ()
        pure r

    close = manualCloseConnection

    send connVar@(D.TcpConnectionVar closeSignal closedSignal sockVar) msg
        | length msg > D.packetSize = trace @String "[send] Too big message" $ pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- trace @String "[send] Taking sock" $ atomically $ takeTMVar sockVar
            err <- trace @String "[send] Sending data" $ try $ S.sendAll sock msg
            res <- case err of
                Right _                     -> pure $ Right ()
                Left  (err :: SomeException)  -> do
                    trace @String ("[send] exc got, closing " <> show err) $ manualCloseConnection' connVar
                    pure $ Left D.ConnectionClosed
            trace @String "[send] Releasing sock" $ atomically (putTMVar sockVar sock)
            pure res

manualCloseConnection connVar@(D.TcpConnectionVar closeSignal closedSignal sockVar) = do
    sock <- trace @String "[manualCloseConnection] Taking sock" $ atomically $ takeTMVar sockVar
    manualCloseConnection' connVar
    trace @String "[manualCloseConnection] Releasing sock" $ atomically (putTMVar sockVar sock)

manualCloseConnection' (D.TcpConnectionVar closeSignal closedSignal _) = do
    trace @String "[manualCloseConnection] start" $ pure ()
    trace @String "[manualCloseConnection] writing CloseSignal" $ atomically $ writeTVar closeSignal True
    trace @String "[manualCloseConnection] waiting for ClosedSignal" $ atomically $ takeTMVar closedSignal


getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket
