{-# LANGUAGE    LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Framework.Networking.Internal.Udp.Connection
    ( close
    , send
    , startServer
    , stopServer
    , openConnect
    , sendUdpMsg
    ) where

import           Enecuum.Prelude
import qualified Data.Map as M
import           Enecuum.Framework.Networking.Internal.Connection
import           Data.Aeson
-- import           Control.Concurrent.Chan
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar

-- import           Data.Aeson.Lens
import           Control.Concurrent.Async
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
readingWorker :: (Text -> IO ()) -> TVar Bool -> Handlers D.Udp -> D.Connection D.Udp -> S.Socket -> IO () 
readingWorker logger closeSignal handlers tcpCon sock = do
    needClose <- readTVarIO closeSignal
    unless needClose $ do
        eMsg <- try $ S.recv sock $ toEnum D.packetSize
        case eMsg of
            Left (err :: SomeException) -> logger $ "Error in reading socket: " <> show err
            Right msg -> do
                case decode $ B.fromStrict msg of
                    Just (D.NetworkMsg tag val) ->
                        whenJust (tag `M.lookup` handlers) $ \handler -> handler val tcpCon
                    Nothing  -> logger $ "Error in decoding en msg: " <> show msg
                readingWorker logger closeSignal handlers tcpCon sock


sendUdpMsg :: D.Address -> LByteString -> IO (Either D.NetworkError ())
sendUdpMsg addr msg = if length msg > D.packetSize
    then pure $ Left D.TooBigMessage
    else tryM
        (runClient S.Datagram addr $ \sock -> S.sendAll sock $ B.toStrict msg)
        (pure $ Left D.AddressNotExist)
        (\_ -> pure $ Right ())


runHandler :: D.Connection D.Udp -> Handlers D.Udp -> (Text -> IO ()) -> LByteString -> IO ()
runHandler netConn handlers logger msg = case decode msg of
    Just (D.NetworkMsg tag val) -> whenJust (handlers ^. at tag) $
        \handler -> handler val netConn
    Nothing                     -> logger $ "Error in decoding en msg: " <> show msg


makeUdpCon :: S.Socket -> IO (D.ConnectionVar D.Udp)
makeUdpCon sock = do
    closeSignal <- newTVarIO False
    sockVar     <- newTMVarIO sock
    pure $ D.ClientUdpConnectionVar closeSignal sockVar

instance NetworkConnection D.Udp where
    startServer port handlers insertConnect logger = do
        chan <- atomically newTChan

        void $ forkIO $ runUDPServer chan port $ \socket sockAddr msg -> do
            let host       = D.sockAddrToHost sockAddr
                connection = D.Connection $ D.Address host port

            sockVar     <- newTMVarIO socket
            void $ insertConnect connection (D.ServerUdpConnectionVar sockAddr sockVar)
            runHandler connection handlers logger msg
        pure chan

    send _ msg | length msg > D.packetSize = pure $ Left D.TooBigMessage
    send (D.ClientUdpConnectionVar _ sockVar) msg =
        sendMsg (\sock -> S.sendAll sock (B.toStrict msg)) sockVar
    send (D.ServerUdpConnectionVar sockAddr    sockVar) msg =
        sendMsg (\sock -> S.sendTo sock (B.toStrict msg) sockAddr) sockVar

    close (D.ClientUdpConnectionVar closeSignal _) = writeTVar closeSignal True
    close _  = pure ()

    openConnect addr@(D.Address host port) handlers logger = do
        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- S.socket (S.addrFamily address) S.Datagram S.defaultProtocol
        S.connect sock $ S.addrAddress address

        udpConVar@(D.ClientUdpConnectionVar closeSignal _) <- makeUdpCon sock
        let worker = readingWorker logger closeSignal handlers (D.Connection addr) sock `finally` S.close sock

        void $ forkIO worker
        pure $ Just udpConVar
