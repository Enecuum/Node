{-# LANGUAGE LambdaCase#-}
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

instance NetworkConnection D.Tcp where
    startServer port handlers ins loger = do
        chan <- atomically newTChan
        void $ forkIO $ runTCPServer chan port $ \sock -> do
            addr <- getAdress sock
            conn <- D.TcpConnectionVar <$> atomically (newTMVar =<< newTChan)
            let networkConnecion = D.Connection $ D.Address addr port
            ins networkConnecion conn
            void $ race (runHandlers conn networkConnecion sock handlers loger) (connectManager conn sock)
        pure chan

    openConnect addr handlers loger = do
        conn <- D.TcpConnectionVar <$> atomically (newTMVar =<< newTChan)
        void $ forkIO $ do
            tryML
                (runClient TCP addr $ \wsConn -> void $ race
                    (runHandlers conn (D.Connection addr) wsConn handlers loger)
                    (connectManager conn wsConn))
                (atomically $ closeConn conn)
        pure conn

    close conn = do
        writeComand conn D.Close
        closeConn conn

    send (D.TcpConnectionVar conn) = sendWithTimeOut conn

getAdress :: S.Socket -> IO D.Host
getAdress socket = D.sockAddrToHost <$> S.getSocketName socket

--------------------------------------------------------------------------------
-- * Internal
runHandlers :: D.ConnectionVar D.Tcp -> D.Connection D.Tcp -> S.Socket -> Handlers D.Tcp -> (Text -> IO ()) -> IO ()
runHandlers conn netConn wsConn handlers loger = do
    tryM (S.recv wsConn $ toEnum D.packetSize) (atomically $ closeConn conn) $ \msg -> do
        case decode msg of
            Just val -> callHandler netConn val handlers
            Nothing  -> loger $ "Error in decoding en msg: " <> show msg
        runHandlers conn netConn wsConn handlers loger

callHandler :: D.Connection D.Tcp -> D.NetworkMsg -> Handlers D.Tcp -> IO ()
callHandler conn (D.NetworkMsg tag val) handlers = whenJust (handlers ^. at tag) $ \handler -> handler val conn

-- | Manager for controlling of WS connect.
connectManager :: D.ConnectionVar D.Tcp -> S.Socket -> IO ()
connectManager conn@(D.TcpConnectionVar c) wsConn = readCommand conn >>= \case
    -- close connection
    Just D.Close      -> atomically $ unlessM (isEmptyTMVar c) $ void $ takeTMVar c
    -- send msg to alies node
    Just (D.Send val var) -> do
        tryM (S.sendAll wsConn val) (atomically $ closeConn conn) $ \_ -> do
            atomically $ putTMVar var True
            connectManager conn wsConn
    -- conect is closed, stop of command reading
    Nothing -> pure ()

-- | Read comand to connect manager
readCommand :: D.ConnectionVar D.Tcp -> IO (Maybe D.Comand)
readCommand (D.TcpConnectionVar conn) = atomically $ do
    ok <- isEmptyTMVar conn
    if ok
        then pure Nothing
        else do
            chan <- readTMVar conn
            Just <$> readTChan chan

-- close connection 
closeConn :: D.ConnectionVar D.Tcp -> STM ()
closeConn (D.TcpConnectionVar conn) = unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn

writeComand :: D.ConnectionVar D.Tcp -> D.Comand -> STM ()
writeComand (D.TcpConnectionVar conn) cmd = unlessM (isEmptyTMVar conn) $ do
    chan <- readTMVar conn
    writeTChan chan cmd
