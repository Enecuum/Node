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


type Address  = ConnectInfo
type Handler  = Value -> Connection -> IO ()
type Handlers = Map Text Handler

newtype Connection = Connection (TMVar (TChan Comand))
data Comand where
    Close       :: Comand
    Send        :: Value -> Comand

newtype ServerHandle = ServerHandle (TChan ServerComand)

-- | Start new server witch port
startServer :: PortNumber -> Handlers -> IO ServerHandle
startServer port handlers = do
    chan <- atomically newTChan 
    void $ forkIO $ runServer chan port $ \_ pending -> do
        wsConn <- WS.acceptRequest pending
        conn <- Connection <$> atomically (newTMVar =<< newTChan)
        void $ race
            (runHandlers conn wsConn handlers)
            (connectManager conn wsConn)
    return $ ServerHandle chan

-- | Stop the server
stopServer :: ServerHandle -> IO ()
stopServer (ServerHandle chan)= atomically $ writeTChan chan StopServer

-- | Open new connect to adress
openConnect :: Address -> Handlers -> IO Connection
openConnect (ConnectInfo ip port) handlers = do
    conn <- Connection <$> atomically (newTMVar =<< newTChan) 
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
close :: Connection -> IO ()
close conn = atomically $ do
    writeComand conn Close
    closeConn conn

-- | Send msg to node.
send :: Connection -> Value -> IO ()
send conn msg = atomically $ writeComand conn $ Send msg

--------------------------------------------------------------------------------
-- * Internal
runHandlers :: Connection -> WS.Connection -> Handlers -> IO ()
runHandlers conn wsConn handlers = do
    msg <- try $ WS.receiveData wsConn
    case msg of
        Left (_ :: SomeException) -> atomically $ closeConn conn
        Right rawMsg -> do
            whenJust (decodeStrict rawMsg) $
                \val -> callHandler conn val handlers
            runHandlers conn wsConn handlers

callHandler :: Connection -> Value -> Handlers -> IO ()
callHandler conn val handlers =
    whenJust (val ^? key "tag" . _String) $ \tag ->
        whenJust (handlers^.at tag) $ \handler ->
            handler val conn

-- | Manager for controlling of WS connect.
connectManager :: Connection -> WS.Connection -> IO ()
connectManager conn@(Connection c) wsConn = readCommand conn >>= \case
    -- close connection
    Just Close -> atomically $ unlessM (isEmptyTMVar c)
        $ void $ takeTMVar c
    -- send msg to alies node
    Just (Send val) -> do
        e <- try $ WS.sendTextData wsConn $ encode val
        case e of
            Right _ -> connectManager conn wsConn 
            Left (_ :: SomeException) -> atomically $ closeConn conn
    -- conect is closed, stop of command reading
    Nothing -> return ()

-- | Read comand to connect manager
readCommand :: Connection -> IO (Maybe Comand)
readCommand (Connection conn) = atomically $ do
    ok <- isEmptyTMVar conn
    if ok then return Nothing
    else do
        chan <- readTMVar conn
        Just <$> readTChan chan

-- close connection 
closeConn :: Connection -> STM ()
closeConn (Connection conn) = unlessM (isEmptyTMVar conn) $
    void $ takeTMVar conn

writeComand :: Connection -> Comand -> STM ()
writeComand (Connection conn) cmd =
    unlessM (isEmptyTMVar conn) $ do
        chan <- readTMVar conn
        writeTChan chan cmd