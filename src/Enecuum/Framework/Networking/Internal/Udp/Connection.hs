{-# LANGUAGE LambdaCase#-}
module Enecuum.Framework.Networking.Internal.Udp.Connection
    ( close
    , send
    , startServer
    , stopServer
    , openConnect
    , sendUdpMsg
    ) where

import           Enecuum.Prelude
import           Data.Aeson
import           Control.Concurrent.Chan
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar

import           Enecuum.Legacy.Service.Network.Base
import           Data.Aeson.Lens
import           Control.Concurrent.Async
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client
import           Enecuum.Framework.Networking.Internal.Udp.Server 
import qualified Network.Socket as S hiding (recv, send, sendAll)
import qualified Network.Socket.ByteString.Lazy as S
import           Control.Monad.Extra

type Handler    = Value -> D.Connection D.Udp -> IO ()
type Handlers   = Map Text Handler

type ServerHandle = TChan D.ServerComand
-- ByteString -> (Chan SendMsg) -> SockAddr

-- | Start new server witch port
startServer :: PortNumber -> Handlers -> (D.Connection D.Udp -> D.ConnectionVar D.Udp -> IO ()) -> IO ServerHandle
startServer port handlers insertConnect = do
    chan <- atomically newTChan
    void $ forkIO $ runUDPServer chan port $ \msg msgChan sockAddr -> do

        let host       = D.sockAddrToHost sockAddr
            connection = D.Connection $ D.Address host port

        insertConnect connection (D.ServerUdpConnectionVar sockAddr msgChan)
        runHandlers   connection handlers msg
    pure chan

runHandlers :: D.Connection D.Udp -> Handlers -> LByteString -> IO ()
runHandlers netConn handlers msg =
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (handlers ^. at tag) $ \handler -> handler val netConn

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan D.StopServer

-- | Send msg to node.
send :: D.ConnectionVar D.Udp -> LByteString -> STM ()
send (D.ClientUdpConnectionVar conn)          msg = writeComand conn $ D.Send  msg
send (D.ServerUdpConnectionVar sockAddr chan) msg = writeTChan  chan $ D.SendMsg sockAddr msg

writeComand :: TMVar (TChan D.Comand) -> D.Comand -> STM ()
writeComand conn cmd = unlessM (isEmptyTMVar conn) $ do
    chan <- readTMVar conn
    writeTChan chan cmd

-- | Close the connect
close :: D.ConnectionVar D.Udp -> STM ()
close (D.ClientUdpConnectionVar conn) = writeComand conn D.Close >> closeConn conn
close (D.ServerUdpConnectionVar _ _)  = pure ()

-- close connection
closeConn :: TMVar (TChan D.Comand) -> STM ()
closeConn conn = unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn


-- | Read comand to connect manager
readCommand :: TMVar (TChan D.Comand) -> IO (Maybe D.Comand)
readCommand conn = atomically $ do
    ok <- isEmptyTMVar conn
    if ok
        then pure Nothing
        else do
            chan <- readTMVar conn
            Just <$> readTChan chan

sendUdpMsg :: D.Address -> LByteString -> IO ()
sendUdpMsg addr msg = runClient UDP addr $ \sock -> S.sendAll sock msg

-- | Open new connect to adress
openConnect :: D.Address -> Handlers -> IO (D.ConnectionVar D.Udp)
openConnect addr handlers = do
    conn <- atomically (newTMVar =<< newTChan)
    void $ forkIO $ do
        tryML
            (runClient UDP addr $ \sock -> void $ race
                (readMessages (D.Connection addr) handlers sock)
                (connectManager conn sock))
            (atomically $ closeConn conn)
    pure $ D.ClientUdpConnectionVar conn

readMessages :: D.Connection D.Udp -> Handlers -> S.Socket -> IO ()
readMessages conn handlers sock = tryMR (S.recv sock (1024 * 4)) $ \msg -> do 
    runHandlers conn handlers msg
    readMessages conn handlers sock

-- | Manager for controlling of WS connect.
connectManager :: TMVar (TChan D.Comand) -> S.Socket -> IO ()
connectManager conn sock = readCommand conn >>= \case
    -- close connection
    Just D.Close      -> atomically $ unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn
    -- send msg to alies node
    Just (D.Send val) -> do
        tryM (S.sendAll sock val) (atomically $ closeConn conn) $
            \_ -> connectManager conn sock
    -- conect is closed, stop of command reading
    Nothing           -> pure ()

