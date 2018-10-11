{-# LANGUAGE LambdaCase#-}
module Enecuum.Framework.Networking.Internal.Tcp.Connection
    ( close
    , send
    , startServer
    , stopServer
    , openConnect
    ) where

import           Enecuum.Prelude
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar

import           Enecuum.Legacy.Service.Network.Base
import           Data.Aeson.Lens
import           Control.Concurrent.Async
import           Enecuum.Framework.Runtime (ConnectionVar (..))
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client
import           Enecuum.Framework.Networking.Internal.Tcp.Server 
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)
import           Control.Monad.Extra

type Handler    = Value -> D.TcpConnection -> IO ()
type Handlers   = Map Text Handler

type ServerHandle = TChan D.ServerComand

-- | Start new server witch port
startServer :: PortNumber -> Handlers -> (D.TcpConnection -> ConnectionVar -> IO ()) -> IO ServerHandle
startServer port handlers ins = do
    chan <- atomically newTChan
    void $ forkIO $ runTCPServer chan port $ \sock -> do
        addr <- getAdress sock
        conn <- ConnectionVar <$> atomically (newTMVar =<< newTChan)
        let networkConnecion = D.TcpConnection $ D.Address addr port
        ins networkConnecion conn
        void $ race (runHandlers conn networkConnecion sock handlers) (connectManager conn sock)
    pure chan

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan D.StopServer

-- | Open new connect to adress
openConnect :: D.Address -> Handlers -> IO ConnectionVar
openConnect addr handlers = do
    conn <- ConnectionVar <$> atomically (newTMVar =<< newTChan)
    void $ forkIO $ do
        tryML
            (runClient TCP addr $ \wsConn -> void $ race
                (runHandlers conn (D.TcpConnection addr) wsConn handlers)
                (connectManager conn wsConn))
            (atomically $ closeConn conn)
    pure conn

-- | Close the connect
close :: ConnectionVar -> STM ()
close conn = do
    writeComand conn D.Close
    closeConn conn

-- | Send msg to node.
send :: ConnectionVar -> LByteString -> STM ()
send conn msg = writeComand conn $ D.Send msg

--------------------------------------------------------------------------------
-- * Internal
runHandlers :: ConnectionVar -> D.TcpConnection -> S.Socket -> Handlers -> IO ()
runHandlers conn netConn wsConn handlers = do
    tryM (S.recv wsConn (1024 * 4)) (atomically $ closeConn conn) $ \msg -> do
        whenJust (decode msg) $ \val -> callHandler netConn val handlers
        runHandlers conn netConn wsConn handlers

callHandler :: D.TcpConnection -> D.NetworkMsg -> Handlers -> IO ()
callHandler conn (D.NetworkMsg tag val) handlers = whenJust (handlers ^. at tag) $ \handler -> handler val conn

-- | Manager for controlling of WS connect.
connectManager :: ConnectionVar -> S.Socket -> IO ()
connectManager conn@(ConnectionVar c) wsConn = readCommand conn >>= \case
    -- close connection
    Just D.Close      -> atomically $ unlessM (isEmptyTMVar c) $ void $ takeTMVar c
    -- send msg to alies node
    Just (D.Send val) -> do
        tryM (S.sendAll wsConn val) (atomically $ closeConn conn) $ \_ ->
            connectManager conn wsConn
    -- conect is closed, stop of command reading
    Nothing -> pure ()

-- | Read comand to connect manager
readCommand :: ConnectionVar -> IO (Maybe D.Comand)
readCommand (ConnectionVar conn) = atomically $ do
    ok <- isEmptyTMVar conn
    if ok
        then pure Nothing
        else do
            chan <- readTMVar conn
            Just <$> readTChan chan

-- close connection 
closeConn :: ConnectionVar -> STM ()
closeConn (ConnectionVar conn) = unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn

writeComand :: ConnectionVar -> D.Comand -> STM ()
writeComand (ConnectionVar conn) cmd = unlessM (isEmptyTMVar conn) $ do
    chan <- readTMVar conn
    writeTChan chan cmd
