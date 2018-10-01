{-# LANGUAGE LambdaCase#-}
module Enecuum.Framework.Networking.Internal
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
import qualified Network.WebSockets as WS
import           Enecuum.Legacy.Service.Network.Base
import           Data.Aeson.Lens
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import           Enecuum.Legacy.Refact.Network.Server
import           Control.Concurrent.Async
import qualified Enecuum.Framework.Domain.Networking as D

type Handler  = Value -> Connection -> IO ()
type Handlers = Map Text Handler

type Connection = D.NetworkConnection
type ServerHandle = TChan ServerComand

-- | Start new server witch port
startServer :: PortNumber -> Handlers -> IO ServerHandle
startServer port handlers = do
    chan <- atomically newTChan 
    void $ forkIO $ runServer chan port $ \_ pending -> do
        wsConn <- WS.acceptRequest pending
        conn <- D.NetworkConnection <$> atomically (newTMVar =<< newTChan)
        void $ race
            (runHandlers conn wsConn handlers)
            (connectManager conn wsConn)
    return chan

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan StopServer

-- | Open new connect to adress
openConnect :: D.Address -> Handlers -> IO D.NetworkConnection
openConnect (D.Address ip port) handlers = do
    conn <- D.NetworkConnection <$> atomically (newTMVar =<< newTChan) 
    void $ forkIO $ do
        res <- try $ runClient ip (fromEnum port) "/" $
            \wsConn -> void $ race
                (runHandlers conn wsConn handlers)
                (connectManager conn wsConn)
        case res of
            Right _ -> return ()
            Left (_ :: SomeException) -> atomically $ closeConn conn
    return conn

-- | Close the connect
close :: D.NetworkConnection -> IO ()
close conn = atomically $ do
    writeComand conn D.Close
    closeConn conn

-- | Send msg to node.
send :: D.NetworkConnection -> Value -> IO ()
send conn msg = atomically $ writeComand conn $ D.Send msg

--------------------------------------------------------------------------------
-- * Internal
runHandlers :: D.NetworkConnection -> WS.Connection -> Handlers -> IO ()
runHandlers conn wsConn handlers = do
    msg <- try $ WS.receiveData wsConn
    case msg of
        Left (_ :: SomeException) -> atomically $ closeConn conn
        Right rawMsg -> do
            whenJust (decodeStrict rawMsg) $
                \val -> callHandler conn val handlers
            runHandlers conn wsConn handlers

callHandler :: D.NetworkConnection -> Value -> Handlers -> IO ()
callHandler conn val handlers =
    whenJust (val ^? key "tag" . _String) $ \tag ->
        whenJust (handlers^.at tag) $ \handler ->
            handler val conn

-- | Manager for controlling of WS connect.
connectManager :: D.NetworkConnection -> WS.Connection -> IO ()
connectManager conn@(D.NetworkConnection c) wsConn = readCommand conn >>= \case
    -- close connection
    Just D.Close -> atomically $ unlessM (isEmptyTMVar c)
        $ void $ takeTMVar c
    -- send msg to alies node
    Just (D.Send val) -> do
        e <- try $ WS.sendTextData wsConn $ encode val
        case e of
            Right _ -> connectManager conn wsConn 
            Left (_ :: SomeException) -> atomically $ closeConn conn
    -- conect is closed, stop of command reading
    Nothing -> return ()

-- | Read comand to connect manager
readCommand :: D.NetworkConnection -> IO (Maybe D.Comand)
readCommand (D.NetworkConnection conn) = atomically $ do
    ok <- isEmptyTMVar conn
    if ok then return Nothing
    else do
        chan <- readTMVar conn
        Just <$> readTChan chan

-- close connection 
closeConn :: D.NetworkConnection -> STM ()
closeConn (D.NetworkConnection conn) = unlessM (isEmptyTMVar conn) $
    void $ takeTMVar conn

writeComand :: D.NetworkConnection -> D.Comand -> STM ()
writeComand (D.NetworkConnection conn) cmd =
    unlessM (isEmptyTMVar conn) $ do
        chan <- readTMVar conn
        writeTChan chan cmd