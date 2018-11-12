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
import qualified Network.Socket as S hiding (recv, send, sendTo, sendAll)
import qualified Network.Socket.ByteString as S
import           Control.Monad.Extra
import           Data.ByteString.Lazy         as B (fromStrict, toStrict)

sendMsg sendFunc sockVar = do
    sock <- atomically $ takeTMVar sockVar
    err <- try $ sendFunc sock
    atomically (putTMVar sockVar sock)
    pure $ case err of
        Right _                     -> Right ()
        Left  (_ :: SomeException)  -> Left D.ConnectionClosed

--
-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: TVar Bool -> Conn.Handlers D.Udp -> D.Connection D.Udp -> S.Socket -> IO ()
readingWorker closeSignal handlers tcpCon sock = do
    needClose <- readTVarIO closeSignal
    unless needClose $ do
        eMsg <- try $ S.recv sock $ toEnum D.packetSize
        case eMsg of
            Left (err :: SomeException) -> pure ()
            Right msg -> do
                case decode $ B.fromStrict msg of
                    Just (D.NetworkMsg tag val) ->
                        whenJust (tag `M.lookup` handlers) $ \handler -> handler val tcpCon
                    Nothing  -> pure ()
                readingWorker closeSignal handlers tcpCon sock


sendUdpMsg :: D.Address -> LByteString -> IO (Either D.NetworkError ())
sendUdpMsg addr msg = if length msg > D.packetSize
    then pure $ Left D.TooBigMessage
    else tryM
        (runClient S.Datagram addr $ \sock -> S.sendAll sock $ B.toStrict msg)
        (pure $ Left D.AddressNotExist)
        (\_ -> pure $ Right ())


runHandler :: D.Connection D.Udp -> Conn.Handlers D.Udp -> LByteString -> IO ()
runHandler netConn handlers msg = case decode msg of
    Just (D.NetworkMsg tag val) -> whenJust (handlers ^. at tag) $
        \handler -> handler val netConn
    Nothing                     -> pure ()


makeUdpCon :: S.Socket -> IO (Conn.NativeConnection D.Udp)
makeUdpCon sock = do
    closeSignal <- newTVarIO False
    sockVar     <- newTMVarIO sock
    pure $ Conn.ClientUdpConnection closeSignal sockVar 

-- TODO: rework all of UDP according to TCP sample.
instance Conn.NetworkConnection D.Udp where
    startServer port handlers (Conn.ConnectionRegister addConn _) = do
        chan <- atomically newTChan

        void $ forkIO $ runUDPServer chan port $ \socket sockAddr msg -> do
            let host       = D.sockAddrToHost sockAddr

            -- TODO: use a real bound address
            let conn = D.Connection $ D.BoundAddress $ D.Address host port

            sockVar <- newTMVarIO socket
            addConn (Conn.ServerUdpConnection sockAddr sockVar conn)
            runHandler conn handlers msg
        pure $ Just $ Conn.OldServerHandle chan

    send _ msg | length msg > D.packetSize = pure $ Left D.TooBigMessage
    send (Conn.ClientUdpConnection _ sockVar) msg =
        sendMsg (\sock -> S.sendAll sock (B.toStrict msg)) sockVar
    send (Conn.ServerUdpConnection sockAddr sockVar _) msg =
        sendMsg (\sock -> S.sendTo sock (B.toStrict msg) sockAddr) sockVar

    close (Conn.ClientUdpConnection closeSignal _) = atomically $ writeTVar closeSignal True
    close _  = pure ()

    open addr@(D.Address host port) handlers = do
        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- S.socket (S.addrFamily address) S.Datagram S.defaultProtocol
        S.connect sock $ S.addrAddress address

        udpConVar@(Conn.ClientUdpConnection closeSignal _) <- makeUdpCon sock

        -- TODO: use a real bound address
        let worker = readingWorker closeSignal handlers (D.Connection (D.BoundAddress addr)) sock `finally` S.close sock

        void $ forkIO worker
        pure $ Just udpConVar
