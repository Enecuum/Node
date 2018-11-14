{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Framework.Networking.Internal.Udp.Connection
    ( sendUdpMsg
    ) where

import           Enecuum.Prelude

import           Control.Monad.Extra
import qualified Data.Map as M
import           Data.Aeson
import           Data.ByteString.Lazy      as B (fromStrict, toStrict)
import qualified Network.Socket            as S hiding (recv, send, sendTo, sendAll, recvFrom)
import qualified Network.Socket.ByteString as S

import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import           Enecuum.Framework.Runtime as R
import qualified Enecuum.Core.Runtime      as R
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client

sendMsg
    :: (Conn.NetworkConnection protocol, Show a)
    => R.RuntimeLogger
    -> (S.Socket -> IO a)
    -> NativeConnection protocol
    -> IO (Either D.NetworkError ())
sendMsg logger sendFunc conn = do
    let sockVar = R.getSocketVar conn
    sock <- atomically $ takeTMVar sockVar
    err <- try $ sendFunc sock
    atomically (putTMVar sockVar sock)
    case err of
        Right _                     -> pure $ Right ()
        Left  (_ :: SomeException)  -> do
            R.logError' logger (show err)
            Conn.close logger conn
            pure $ Left D.ConnectionClosed

-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?

analyzeReadingResult
    :: R.Handlers D.Udp
    -> D.Connection D.Udp
    -> Either SomeException LByteString
    -> IO WorkerAction
analyzeReadingResult _ _  (Left err)            = pure (WFinish, WError $ show err)
analyzeReadingResult _ _ (Right msg) | null msg = pure (WFinish, WOk)
analyzeReadingResult handlers conn (Right msg) = do
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (tag `M.lookup` handlers) $ \handler ->
            handler val conn
    pure (WContinue, WOk)

readingWorker :: R.RuntimeLogger -> R.Handlers D.Udp -> D.Connection D.Udp -> S.Socket -> IO ()
readingWorker logger handlers conn sock = do
    isDead <- Conn.isSocketClosed sock
    unless isDead $ do
        eRead <- try $ S.recv sock $ toEnum D.packetSize
        action <- analyzeReadingResult handlers conn (B.fromStrict <$> eRead)
        withWorkerAction logger action $ readingWorker logger handlers conn sock

sendUdpMsg :: D.Address -> LByteString -> IO (Either D.NetworkError ())
sendUdpMsg _    msg | length msg > D.packetSize = pure $ Left D.TooBigMessage
sendUdpMsg addr msg = tryM
    (runClient S.Datagram addr $ \sock -> S.sendAll sock $ B.toStrict msg)
    (pure $ Left D.AddressNotExist)
    (\_ -> pure $ Right ())

serverWorker
    :: ConnectCounter
    -> Conn.ConnectionRegister D.Udp
    -> Handlers D.Udp
    -> TMVar S.Socket
    -> IO b
serverWorker connectCounter (Conn.ConnectionRegister addConn _) handlers sockVar = do
    sock <- atomically $ readTMVar sockVar
    forever $ tryMR (S.recvFrom sock D.packetSize) $ \(rawMsg, sockAddr) -> do
        connectId <- Conn.getNewConnectId connectCounter
        let message = B.fromStrict rawMsg
        let conn    = D.Connection (D.BoundAddress $ Conn.unsafeFromSockAddr sockAddr) connectId
        addConn (Conn.ServerUdpConnection sockAddr sockVar conn)
        runHandler conn handlers message

runHandler :: D.Connection D.Udp -> R.Handlers D.Udp -> LByteString -> IO ()
runHandler netConn handlers msg =
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (handlers ^. at tag) $ \handler ->
            handler val netConn

listenUDP :: S.PortNumber -> IO S.Socket
listenUDP port = do
    serveraddr:_ <- S.getAddrInfo
        (Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
        Nothing
        (Just $ show port)
    sock <- S.socket (S.addrFamily serveraddr) S.Datagram S.defaultProtocol
    S.bind sock (S.addrAddress serveraddr)
    pure sock


instance Conn.NetworkConnection D.Udp where
    startServer logger connectCounter port handlers register = do
        eListenSock <- try $ listenUDP port
        case eListenSock of
            Left (err :: SomeException) -> R.logError' logger (show err) >> pure Nothing
            Right listenSock -> do
                listenSockVar  <- newTMVarIO listenSock
                let worker = serverWorker connectCounter register handlers listenSockVar
                        `finally` Conn.closeSocket listenSock
                serverWorkerId <- forkIO worker 
                pure $ Just $ R.ServerHandle listenSockVar serverWorkerId

    send logger _ msg | length msg > D.packetSize = do
        R.logError' logger $ "Message too big (> " <> show D.packetSize <> "): " <> show msg
        pure $ Left D.TooBigMessage
    send logger conn@(Conn.ServerUdpConnection sockAddr _ _) msg =
        sendMsg logger (\sock -> S.sendTo sock (B.toStrict msg) sockAddr) conn
    send logger conn msg = sendMsg logger (\sock -> S.sendAll sock (B.toStrict msg)) conn

    close _ conn@Conn.ClientUdpConnection{} = Conn.closeConnection conn
    close _ _  = pure ()

    open logger counter (D.Address host port) handlers = do
        address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock       <- S.socket (S.addrFamily address) S.Datagram S.defaultProtocol
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
                let worker = readingWorker logger handlers conn sock `finally` S.close sock
                readerId <- forkIO worker
                pure $ Just $ Conn.ClientUdpConnection readerId sockVar conn

