{-# LANGUAGE     LambdaCase       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Framework.Networking.Internal.Tcp.Connection where

import           Enecuum.Prelude
import           Enecuum.Framework.Networking.Internal.Connection
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar

import           Control.Concurrent.Async
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client
import           Enecuum.Framework.Networking.Internal.Tcp.Server 
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)
import           Control.Monad.Extra

    -- TODO: what is the behavior when the message > packetSize? What's happening with its rest?
    -- Will it garbage the next message?

readingWorker logger closeSignal handlers sock = do
    needClose <- readTVarIO closeSignal
    unless needClose $ do
        eMsg <- try $ S.recv sock $ toEnum D.packetSize
        case eMsg of
            Left (err :: SomeException) -> logger $ "Error in reading socket: " <> show msg
            Right msg -> do
                case decode msg of
                    Just val -> callHandler netConn val handlers
                    Nothing  -> logger $ "Error in decoding en msg: " <> show msg
                readingWorker logger closeSignal handlers sock

instance NetworkConnection D.Tcp where
    startServer port handlers registerConnection logger = do
        chan <- atomically newTChan
        void $ forkIO $ runTCPServer chan port $ \sock -> do
            addr <- getAdress sock
            
            conn <- D.TcpConnectionVar <$> atomically ()
            let networkConnecion = D.Connection $ D.Address addr port
            registerConnection networkConnecion conn
            void $ race (runHandlers conn networkConnecion sock handlers logger) (connectManager conn sock)
        pure chan

    openConnect addr handlers logger = do
        -- TODO: exceptions

        -- Always returns non-empty list
        address <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
        sock    <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        S.connect sock $ S.addrAddress address

        closeSignal <- newTVarIO False
        let worker = readingWorker logger closeSignal handlers sock `finally` S.close sock

        readingWorkerId <- forkIO worker

        D.TcpConnectionVar readingWorkerId closeSignal
            <$> newTMVarIO sock

    close (D.TcpConnectionVar rWorkerId closeSignal sock) =
        atomically $ writeTVar closeSignal True

    send (D.TcpConnectionVar conn) msg
        | length msg <= D.packetSize = sendWithTimeOut conn msg
        | otherwise                  = pure $ Left D.TooBigMessage

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket

--------------------------------------------------------------------------------
-- * Internal
-- runHandlers :: D.ConnectionVar D.Tcp -> D.Connection D.Tcp -> S.Socket -> Handlers D.Tcp -> (Text -> IO ()) -> IO ()
-- runHandlers conn netConn wsConn handlers logger =
--     tryM (S.recv wsConn $ toEnum D.packetSize) (atomically $ closeConn conn) $ \msg ->
--         if null msg
--             then atomically $ closeConn conn
--             else do
--                 case decode msg of
--                     Just val -> callHandler netConn val handlers
--                     Nothing  -> logger $ "Error in decoding en msg: " <> show msg
--                 runHandlers conn netConn wsConn handlers logger

callHandler :: D.Connection D.Tcp -> D.NetworkMsg -> Handlers D.Tcp -> IO ()
callHandler conn (D.NetworkMsg tag val) handlers = whenJust (handlers ^. at tag) $ \handler -> handler val conn

-- | Manager for controlling of WS connect.
connectManager :: D.ConnectionVar D.Tcp -> S.Socket -> IO ()
connectManager conn@(D.TcpConnectionVar chan) wsConn = do
    cmd <- atomically $ readTChan chan
    case cmd of
        -- close connection
        D.Close        -> atomically $ unlessM (isEmptyTMVar c) $ void $ takeTMVar c
        -- send msg to alies node
        D.Send val var ->
            tryM (S.sendAll wsConn val) (atomically $ closeConn conn) $ \_ -> do
                _ <- tryPutMVar var True
                connectManager conn wsConn

-- close connection 
-- closeConn :: D.ConnectionVar D.Tcp -> STM ()
-- closeConn (D.TcpConnectionVar conn) = unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn

writeComand :: D.ConnectionVar D.Tcp -> D.Command -> STM ()
writeComand (D.TcpConnectionVar conn) cmd = unlessM (isEmptyTMVar conn) $ do
    chan <- readTMVar conn
    writeTChan chan cmd