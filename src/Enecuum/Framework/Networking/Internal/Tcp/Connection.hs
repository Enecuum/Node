{-# OPTIONS_GHC -fno-warn-orphans #-}

module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude

import           Data.Aeson
import qualified Data.Map as M
import           Network.Socket hiding (recvFrom)
import           Network (PortID (..), listenOn)

import           Enecuum.Framework.Domain  as D
import           Enecuum.Framework.Runtime as R
import qualified Enecuum.Core.Runtime      as R
import           Enecuum.Framework.Runtime (WorkerState (..), WorkerAction, withWorkerAction)
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)

analyzeReadingResult
    :: R.Handlers D.Tcp
    -> D.Connection D.Tcp
    -> Either SomeException LByteString
    -> IO WorkerAction
analyzeReadingResult _        _    (Left err)             = pure (WFinish, WError $ show err)
analyzeReadingResult _        _    (Right msg) | null msg = pure (WFinish, WOk)
analyzeReadingResult handlers conn (Right msg) = case decode msg of
    Nothing                     -> pure (WContinue, WWarning $ "Decoding error: " <> show msg)
    Just (D.NetworkMsg tag val) -> case tag `M.lookup` handlers of
        Nothing      -> pure (WContinue, WWarning $ "Handler not found: " <> tag)
        Just handler -> handler val conn >> pure (WContinue, WOk)

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: R.RuntimeLogger -> R.Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO ()
readingWorker logger handlers conn sock = do
    isDead <- Conn.isSocketClosed sock
    unless isDead $ do
        eRead  <- try $ S.recv sock $ toEnum D.packetSize
        action <- analyzeReadingResult handlers conn eRead
        withWorkerAction logger action $ readingWorker logger handlers conn sock

acceptConnects :: Socket -> ((Socket, D.BoundAddress) -> IO ()) -> IO ()
acceptConnects listenSock handler = do
    (connSock, boundAddr') <- accept listenSock
    let boundAddr = D.BoundAddress $ Conn.unsafeFromSockAddr boundAddr'
    void $ forkIO $ handler (connSock, boundAddr)

analyzeAcceptingResult :: Either SomeException () -> IO WorkerAction
analyzeAcceptingResult (Left err) = pure (WFinish, WError $ show err)
analyzeAcceptingResult (Right ()) = pure (WContinue, WOk)

acceptingWorker :: R.RuntimeLogger -> Socket -> ((Socket, BoundAddress) -> IO ()) -> IO ()
acceptingWorker logger listenSock handler = do
    eRes   <- try $ acceptConnects listenSock handler
    action <- analyzeAcceptingResult eRes
    withWorkerAction logger action $ acceptingWorker logger listenSock handler

acceptingHandler
    :: R.RuntimeLogger
    -> R.ConnectCounter
    -> Conn.ConnectionRegister Tcp
    -> R.Handlers Tcp
    -> (Socket, BoundAddress)
    -> IO ()
acceptingHandler logger counter (Conn.ConnectionRegister addConn removeConn) handlers (connSock, boundAddr) = do
    connectId <- Conn.getNewConnectId counter
    let conn = D.Connection boundAddr connectId
    let worker = readingWorker logger handlers conn connSock
            `finally` (removeConn conn >> Conn.closeSocket connSock)
    readerId <- forkIO worker
    sockVar <- newTMVarIO connSock
    let nativeConn = Conn.TcpConnection sockVar readerId conn
    addConn nativeConn

instance Conn.NetworkConnection D.Tcp where
    startServer logger counter port handlers register = do
        eListenSock <- try $ listenOn (PortNumber port)
        case eListenSock of
            Left (err :: SomeException) -> R.logError' logger (show err) >> pure Nothing
            Right listenSock -> do
                let handler = acceptingHandler logger counter register handlers
                let worker = acceptingWorker logger listenSock handler
                                `finally` Conn.closeSocket listenSock

                acceptWorkerId <- forkIO worker
                listenSockVar <- newTMVarIO listenSock
                pure $ Just $ R.ServerHandle listenSockVar acceptWorkerId

    open logger counter (D.Address host port) handlers = do
        address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock       <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        eConnected <- try $ S.connect sock $ S.addrAddress address

        case eConnected of
            Left (err :: SomeException) -> do
                R.logError' logger (show err)
                S.close sock
                pure Nothing
            Right () -> do
                connectId <- Conn.getNewConnectId counter
                boundAddr <- D.BoundAddress . Conn.unsafeFromSockAddr <$> S.getSocketName sock
                let conn = D.Connection boundAddr connectId
                sockVar <- newTMVarIO sock
                let worker = readingWorker logger handlers conn sock
                        `finally` Conn.closeSocket sock
                readerId <- forkIO worker
                pure $ Just $ Conn.TcpConnection sockVar readerId conn

    close _ = Conn.closeConnection

    send logger (Conn.TcpConnection sockVar readerId _) msg
        | length msg > D.packetSize = pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- atomically $ takeTMVar sockVar
            eRes <- try $ S.sendAll sock msg
            res <- case eRes of
                Right _                     -> pure $ Right ()
                Left  (err :: SomeException)  -> do
                    R.logError' logger (show err)
                    Conn.closeConnection' sock readerId
                    pure $ Left D.ConnectionClosed
            atomically (putTMVar sockVar sock)
            pure res
