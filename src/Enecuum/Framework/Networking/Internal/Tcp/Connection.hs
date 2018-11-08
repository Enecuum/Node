{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude
import           Enecuum.Framework.Networking.Internal.Connection
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import qualified Data.Map as M


import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Tcp.Server 
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)


-- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
-- Will it garbage the next message?
readingWorker :: (Text -> IO ()) -> TVar Bool -> Handlers D.Tcp -> D.Connection D.Tcp -> S.Socket -> IO () 
readingWorker logger closeSignal handlers tcpCon sock = do
    needClose <- readTVarIO closeSignal
    unless needClose $ do
        eMsg <- try $ S.recv sock $ toEnum D.packetSize
        case eMsg of
            Right msg | not $ null msg -> do
                case decode msg of
                    Just (D.NetworkMsg tag val) ->
                        whenJust (tag `M.lookup` handlers) $ \handler -> handler val tcpCon
                    Nothing  -> logger $ "Error in decoding en msg: " <> show msg
                readingWorker logger closeSignal handlers tcpCon sock
            Left (err :: SomeException) -> logger $ "Error in reading socket: " <> show err
            _                           -> logger "Empty msg (connection closed)."

makeTcpCon :: S.Socket -> IO (D.ConnectionVar D.Tcp)
makeTcpCon sock = do
    closeSignal <- newTVarIO False
    sockVar     <- newTMVarIO sock
    pure $ D.TcpConnectionVar closeSignal sockVar

instance NetworkConnection D.Tcp where
    startServer port handlers registerConnection logger = do
        chan <- atomically newTChan
        void $ forkIO $ runTCPServer chan port $ \sock -> do
            addr     <- getAdress sock
            sockPort <- S.socketPort sock
            tcpConVar@(D.TcpConnectionVar closeSignal _) <- makeTcpCon sock
            let tcpCon = D.Connection $ D.Address addr sockPort
            ok <- registerConnection tcpCon tcpConVar
            when ok $ readingWorker logger closeSignal handlers tcpCon sock
            unless ok $ logger "Connection is refused"
            S.close sock

        pure chan

    openConnect addr@(D.Address host port) handlers logger = do
        -- TODO: exceptions

        -- Always returns non-empty list
        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        ok <- try $ S.connect sock $ S.addrAddress address
        case ok of
            Left (_ :: SomeException) -> do
                S.close sock 
                pure Nothing
            Right _ -> do
                tcpConVar@(D.TcpConnectionVar closeSignal _) <- makeTcpCon sock
                let worker = readingWorker logger closeSignal handlers (D.Connection addr) sock `finally` S.close sock

                void $ forkIO worker
                pure $ Just tcpConVar

    close (D.TcpConnectionVar closeSignal _) = writeTVar closeSignal True

    send (D.TcpConnectionVar closeSignal sockVar) msg
        | length msg > D.packetSize = pure $ Left D.TooBigMessage
        | otherwise                 = do
            sock <- atomically $ takeTMVar sockVar
            err <- try $ S.sendAll sock msg
            res <- case err of
                Right _                     -> pure $ Right ()
                Left  (_ :: SomeException)  -> do
                    atomically $ writeTVar closeSignal True
                    pure $ Left D.ConnectionClosed
            atomically (putTMVar sockVar sock)
            pure res
        

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket

