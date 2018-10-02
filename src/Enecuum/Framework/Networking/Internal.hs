{-# LANGUAGE LambdaCase#-}
module Enecuum.Framework.Networking.Internal
    ( close
    , send
    , startServer
    , stopServer
    , openConnect
    ) where

import           Data.IP
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

type Handler    = Value -> D.NetworkConnection -> IO ()
type Handlers   = Map Text Handler

type ServerHandle = TChan ServerComand

-- | Start new server witch port
startServer
    :: PortNumber
    -> Handlers
    -> (D.NetworkConnection -> D.ConnectionImplementation -> IO ())
    -> IO ServerHandle
startServer port handlers ins = do
    chan <- atomically newTChan 
    void $ forkIO $ runServer chan port $ \addr pending -> do
        wsConn <- WS.acceptRequest pending
        conn <- D.ConnectionImplementation <$> atomically (newTMVar =<< newTChan)
         
        let networkConnecion = D.NetworkConnection $ D.Address (show $ fromHostAddress addr) port
        ins networkConnecion conn
        void $ race
            (runHandlers conn networkConnecion wsConn handlers)
            (connectManager conn wsConn)
    return chan

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan StopServer

-- | Open new connect to adress
openConnect :: D.Address -> Handlers -> IO D.ConnectionImplementation
openConnect addr@(D.Address ip port) handlers = do
    conn <- D.ConnectionImplementation <$> atomically (newTMVar =<< newTChan) 
    void $ forkIO $ do
        res <- try $ runClient ip (fromEnum port) "/" $
            \wsConn -> void $ race
                (runHandlers conn (D.NetworkConnection addr) wsConn handlers)
                (connectManager conn wsConn)
        case res of
            Right _ -> return ()
            Left (_ :: SomeException) -> atomically $ closeConn conn
    return conn

-- | Close the connect
close :: D.ConnectionImplementation -> STM ()
close conn = do
    writeComand conn D.Close
    closeConn conn

-- | Send msg to node.
send :: D.ConnectionImplementation -> LByteString -> STM ()
send conn msg = writeComand conn $ D.Send msg

--------------------------------------------------------------------------------
-- * Internal
runHandlers :: D.ConnectionImplementation -> D.NetworkConnection -> WS.Connection -> Handlers -> IO ()
runHandlers conn netConn wsConn handlers = do
    msg <- try $ WS.receiveData wsConn
    case msg of
        Left (_ :: SomeException) -> atomically $ closeConn conn
        Right rawMsg -> do
            whenJust (decodeStrict rawMsg) $
                \val -> callHandler netConn val handlers
            runHandlers conn netConn wsConn handlers

callHandler :: D.NetworkConnection -> Value -> Handlers -> IO ()
callHandler conn val handlers =
    whenJust (val ^? key "tag" . _String) $ \tag ->
        whenJust (handlers^.at tag) $ \handler ->
            handler val conn

-- | Manager for controlling of WS connect.
connectManager :: D.ConnectionImplementation -> WS.Connection -> IO ()
connectManager conn@(D.ConnectionImplementation c) wsConn = readCommand conn >>= \case
    -- close connection
    Just D.Close -> atomically $ unlessM (isEmptyTMVar c)
        $ void $ takeTMVar c
    -- send msg to alies node
    Just (D.Send val) -> do
        e <- try $ WS.sendTextData wsConn val
        case e of
            Right _ -> connectManager conn wsConn 
            Left (_ :: SomeException) -> atomically $ closeConn conn
    -- conect is closed, stop of command reading
    Nothing -> return ()

-- | Read comand to connect manager
readCommand :: D.ConnectionImplementation -> IO (Maybe D.Comand)
readCommand (D.ConnectionImplementation conn) = atomically $ do
    ok <- isEmptyTMVar conn
    if ok then return Nothing
    else do
        chan <- readTMVar conn
        Just <$> readTChan chan

-- close connection 
closeConn :: D.ConnectionImplementation -> STM ()
closeConn (D.ConnectionImplementation conn) = unlessM (isEmptyTMVar conn) $
    void $ takeTMVar conn

writeComand :: D.ConnectionImplementation -> D.Comand -> STM ()
writeComand (D.ConnectionImplementation conn) cmd =
    unlessM (isEmptyTMVar conn) $ do
        chan <- readTMVar conn
        writeTChan chan cmd