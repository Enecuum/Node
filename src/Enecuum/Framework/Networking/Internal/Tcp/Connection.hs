{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude
import           Enecuum.Framework.Networking.Internal.Connection
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.Async (race) 
import qualified Data.Map as M


import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Tcp.Server 
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)

data ReadingResult = Looping | Reading (Either SomeException LByteString)
data WorkerLoopAction  = ReadNext | FinishReading

readingTimeout = 1000 * 100

analyzeReadingResult
    :: (Text -> IO ())
    -> Handlers D.Tcp
    -> D.Connection D.Tcp
    -> ReadingResult
    -> IO WorkerLoopAction
analyzeReadingResult logger handlers tcpCon Looping                           = do
    trace @String "[readingWorker] looping got after timeout" $ pure ""
    pure ReadNext

analyzeReadingResult logger handlers tcpCon (Reading (Left err))              = do
    trace @String "[readingWorker] exc in receiving data" $ pure ""
    -- logger $ "Error in reading socket: " <> show err
    pure FinishReading

analyzeReadingResult logger handlers tcpCon (Reading (Right msg)) | null msg  = do
    trace @String "[readingWorker] empty data got" $ pure ""
    -- logger "Empty msg (connection closed)."
    pure FinishReading

analyzeReadingResult logger handlers tcpCon (Reading (Right msg)) | otherwise = do
    trace @String "[readingWorker] message read" $ pure ()
    case decode msg of
        Just (D.NetworkMsg tag val) -> do
            trace @String "[readingWorker] calling handler" $ pure ""
            whenJust (tag `M.lookup` handlers) $ \handler -> handler val tcpCon
            trace @String "[readingWorker] done calling handler" $ pure ()
        Nothing  -> do
            trace @String "[readingWorker] decode failed" $ pure ()
            -- logger $ "Error in decoding en msg: " <> show msg
    pure ReadNext

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: (Text -> IO ()) -> TVar Bool -> Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO () 
readingWorker logger closeSignal handlers !tcpCon !sock = do
    needClose <- trace @String "[readingWorker] checking close signal" $ readTVarIO closeSignal
    when needClose $ trace @String "[readingWorker] need close" $ pure ()
    unless needClose $ do
        let loopDelay  = threadDelay readingTimeout >> pure Looping
        let tryReading :: IO (Either SomeException LByteString) = try $ S.recv sock $ toEnum D.packetSize
        let reading :: IO ReadingResult = Reading <$> tryReading
        
        readOrLoopResult :: (Either ReadingResult ReadingResult) <- trace @String "[readingWorker] receiving data" $ race loopDelay reading
        let readingResult = either id id readOrLoopResult
        loopAct <- trace @String "[readingWorker] analyzing reading result" $ analyzeReadingResult logger handlers tcpCon readingResult
        case loopAct of
            ReadNext      -> trace @String "[readingWorker] receiving data again" $ readingWorker logger closeSignal handlers tcpCon sock
            FinishReading -> trace @String "[readingWorker] finishing" $ pure ()

makeTcpCon :: S.Socket -> IO (D.ConnectionVar D.Tcp)
makeTcpCon sock = do
    closeSignal  <- newTVarIO False
    closedSignal <- newTMVarIO ()
    sockVar      <- newTMVarIO sock
    pure $ D.TcpConnectionVar closeSignal closedSignal sockVar


-- runTCPServer :: TChan D.ServerComand -> PortNumber -> (Socket -> IO ()) -> IO ()
-- runTCPServer chan port handler =
--     bracket ((listenOn . PortNumber) port) close $ \sock ->
--         finally (serv chan (acceptConnects sock handler)) (close sock)

-- serv :: TChan a -> IO b -> IO ()
-- serv chan f = void $ race (void $ atomically $ readTChan chan) f

-- acceptConnects :: forall a b . Socket -> (Socket -> IO a) -> IO b
-- acceptConnects sock handler = forever $ do
--     (conn, _) <- accept sock
--     void $ forkFinally (handler conn) (\_ -> close conn)


instance NetworkConnection D.Tcp where
    startServer port handlers registerConnection logger = do
        trace @String "[startServer] start" $ pure ()
        chan <- atomically newTChan
        trace @String "[startServer] forking server" $ pure ()
        void $ forkIO $ runTCPServer chan port $ \sock -> do
            trace @String "[startServer-worker] start, getting network things" $ pure ()
            addr     <- getAdress sock
            sockPort <- S.socketPort sock
            tcpConVar@(D.TcpConnectionVar closeSignal closedSignal _) <- makeTcpCon sock
            let tcpCon = D.Connection $ D.Address addr sockPort
            trace @String "[startServer-worker] registering conn" $ pure ()
            ok <- registerConnection tcpCon tcpConVar
            when ok $ do
                trace @String "[startServer-worker] starting reading worker" $ pure ()
                readingWorker logger closeSignal handlers tcpCon sock
                    `finally` (do
                        trace @String "[startServer-worker] closing sock: worker finished" $ S.close sock
                        atomically $ putTMVar closedSignal ()
                        )
            unless ok $ (do
                -- logger "Connection is refused"
                trace @String "[startServer-worker] closing sock: connection not registered" $ pure ()
                S.close sock
                )

        trace @String "[startServer] done" $ pure ()
        pure chan

    openConnect addr@(D.Address host port) handlers logger = do
        trace @String "[openConnect] start" $ pure ()

        -- Always returns non-empty list
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
                        readingWorker logger closeSignal handlers (D.Connection addr) sock 
                        `finally` (do
                            trace @String "[openConnect] closing sock: worker finished" $ S.close sock
                            atomically $ putTMVar closedSignal ()
                            )

                void $ forkIO worker
                pure $ Just tcpConVar
        trace @String "[openConnect] done" $ pure ()
        pure r

    close (D.TcpConnectionVar closeSignal closedSignal _) = do
        trace @String "[close] start" $ pure ()
        trace @String "[close] writing CloseSignal" $ writeTVar closeSignal True
        trace @String "[close] waiting for ClosedSignal" $ takeTMVar closedSignal
        trace @String "[close] done" $ pure ()



    send connVar@(D.TcpConnectionVar closeSignal closedSignal sockVar) msg
        | length msg > D.packetSize = trace @String "[send] Too big message" $ pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- trace @String "[send] Taking sock" $ atomically $ takeTMVar sockVar
            err <- trace @String "[send] Sending data" $ try $ S.sendAll sock msg
            res <- case err of
                Right _                     -> pure $ Right ()
                Left  (_ :: SomeException)  -> do
                    trace @String "[send] exc got, closing" $ atomically $ close connVar
                    pure $ Left D.ConnectionClosed
            trace @String "[send] Releasing sock" $ atomically (putTMVar sockVar sock)
            pure res
        

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket

