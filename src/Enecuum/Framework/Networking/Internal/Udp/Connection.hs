{-# LANGUAGE LambdaCase#-}
module Enecuum.Framework.Networking.Internal.Udp.Connection where
 {-
    ( close
    , send
    , startServer
    , stopServer
    , openConnect
    ) where
-}
import           Enecuum.Prelude
import           Data.Aeson
import           Control.Concurrent.Chan
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar

import           Enecuum.Legacy.Service.Network.Base
import           Data.Aeson.Lens
import           Control.Concurrent.Async
import           Enecuum.Framework.Runtime (ConnectionVar (..))
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client
import           Enecuum.Framework.Networking.Internal.Udp.Server 
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)
import           Control.Monad.Extra

type Handler    = Value -> D.UdpConnection -> IO ()
type Handlers   = Map Text Handler

type ServerHandle = TChan D.ServerComand
-- ByteString -> (Chan SendMsg) -> SockAddr

data UdpConnectionVar
    = ServerUdp S.SockAddr (TChan SendMsg)
    | ClientUdp (TMVar (TChan D.Comand))

-- | Start new server witch port
startServer :: PortNumber -> Handlers -> (D.UdpConnection -> UdpConnectionVar -> IO ()) -> IO ServerHandle
startServer port handlers insertConnect = do
    chan <- atomically newTChan
    void $ forkIO $ runUDPServer chan port $ \msg msgChan sockAddr -> do

        let host       = D.sockAddrToHost sockAddr
            connection = D.UdpConnection $ D.Address host port

        insertConnect connection (ServerUdp sockAddr msgChan)
        runHandlers   connection handlers msg
    pure chan

runHandlers :: D.UdpConnection -> Handlers -> LByteString -> IO ()
runHandlers netConn handlers msg =
    whenJust (decode msg) $ \(D.NetworkMsg tag val) ->
        whenJust (handlers ^. at tag) $ \handler -> handler val netConn

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan D.StopServer

-- | Send msg to node.
send :: UdpConnectionVar -> LByteString -> STM ()
send (ClientUdp conn)          msg = writeComand conn $ D.Send  msg
send (ServerUdp sockAddr chan) msg = writeTChan  chan $ SendMsg sockAddr msg

writeComand :: TMVar (TChan D.Comand) -> D.Comand -> STM ()
writeComand conn cmd = unlessM (isEmptyTMVar conn) $ do
    chan <- readTMVar conn
    writeTChan chan cmd

-- | Close the connect
close :: UdpConnectionVar -> STM ()
close (ClientUdp conn) = writeComand conn D.Close >> closeConn conn
close (ServerUdp _ _)  = pure ()

-- close connection
closeConn :: TMVar (TChan D.Comand) -> STM ()
closeConn conn = unlessM (isEmptyTMVar conn) $ void $ takeTMVar conn


{-
-- | Open new connect to adress
openConnect :: D.Address -> Handlers -> IO UdpConnectionVar
openConnect addr handlers = do
    conn <- UdpConnectionVar <$> atomically (newTMVar =<< newTChan)
    void $ forkIO $ do
        tryML
            (runClient UDP addr $ \wsConn -> void $ race
                (runHandlers conn (D.UdpConnection addr) wsConn handlers)
                (connectManager conn wsConn))
            (atomically $ closeConn conn)
    pure conn
-}


--------------------------------------------------------------------------------
-- * Internal
{-

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

-}