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
import qualified Network.Socket as S hiding (recv, send)
import qualified Network.Socket.ByteString.Lazy as S
import           Control.Monad.Extra

instance NetworkConnection D.Udp where
    startServer port handlers insertConnect logger = do
        chan <- atomically newTChan
        void $ forkIO $ runUDPServer chan port $ \msg msgChan sockAddr -> do
            let host       = D.sockAddrToHost sockAddr
                connection = D.Connection $ D.Address host port
    
            insertConnect connection (D.ServerUdpConnectionVar sockAddr msgChan)
            runHandlers   connection handlers logger msg
        pure chan

    send (D.ClientUdpConnectionVar conn) msg
        | length msg <= D.packetSize = sendWithTimeOut conn msg
        | otherwise                  = pure $ Left D.TooBigMessage
    send (D.ServerUdpConnectionVar sockAddr chan) msg
        | length msg <= D.packetSize = do
            feedback <- newEmptyMVar
            atomically $ writeTChan chan $ D.SendUdpMsgTo sockAddr msg feedback
            tryTakeResponse 5000 feedback
        | otherwise                  = pure $ Left D.TooBigMessage

    close (D.ClientUdpConnectionVar conn) = writeComand conn D.Close >> closeConn conn
    close (D.ServerUdpConnectionVar _ _)  = pure ()

    openConnect addr handlers logger = do
        conn <- atomically (newTMVar =<< newTChan)
        void $ forkIO $ tryML
            (runClient S.Datagram addr $ \sock -> void $ race
                (readMessages (D.Connection addr) handlers logger sock)
                (connectManager conn sock))
            (atomically $ closeConn conn)
        pure $ D.ClientUdpConnectionVar conn


runHandlers :: D.Connection D.Udp -> Handlers D.Udp -> (Text -> IO ()) -> LByteString -> IO ()
runHandlers netConn handlers logger msg = case decode msg of
    Just (D.NetworkMsg tag val) -> whenJust (handlers ^. at tag) $
        \handler -> handler val netConn
    Nothing                     -> logger $ "Error in decoding en msg: " <> show msg

writeComand :: TMVar (TChan D.Command) -> D.Command -> STM ()
writeComand conn cmd = unlessM (isEmptyTMVar conn) $ do
    chan <- readTMVar conn
    writeTChan chan cmd

-- close connection
closeConn :: TMVar (TChan D.Command) -> STM ()
closeConn conn = unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn

-- | Read comand to connect manager
readCommand :: TMVar (TChan D.Command) -> IO (Maybe D.Command)
readCommand conn = atomically $ do
    ok <- isEmptyTMVar conn
    if ok
        then pure Nothing
        else do
            chan <- readTMVar conn
            Just <$> readTChan chan

sendUdpMsg :: D.Address -> LByteString -> IO (Either D.NetworkError ())
sendUdpMsg addr msg = if length msg > D.packetSize
    then pure $ Left D.TooBigMessage
    else tryM
        (runClient S.Datagram addr $ \sock -> S.sendAll sock msg)
        (pure $ Left D.AddressNotExist)
        (\_ -> pure $ Right ())

readMessages :: D.Connection D.Udp -> Handlers D.Udp -> (Text -> IO ()) -> S.Socket -> IO ()
readMessages conn handlers logger sock = tryMR (S.recv sock $ toEnum D.packetSize) $ \msg -> do 
    runHandlers conn handlers logger msg
    readMessages conn handlers logger sock

-- | Manager for controlling of WS connect.
connectManager :: TMVar (TChan D.Command) -> S.Socket -> IO ()
connectManager conn sock = readCommand conn >>= \case
    -- close connection
    Just D.Close      -> atomically $ unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn
    -- send msg to alies node
    Just (D.Send val feedback) ->
        tryM (S.sendAll sock val) (atomically $ closeConn conn) $
            \_ -> do
                _ <- tryPutMVar feedback True
                connectManager conn sock
    -- conect is closed, stop of command reading
    Nothing           -> pure ()
