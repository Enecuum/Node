{-# LANGUAGE LambdaCase#-}
module Enecuum.Framework.Networking.Internal.Connection where

import           Enecuum.Prelude
import           Data.Aeson
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar

import           Enecuum.Legacy.Service.Network.Base
import           Data.Aeson.Lens
import           Control.Concurrent.Async
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Framework.Networking.Internal.Client
import           Enecuum.Framework.Networking.Internal.Tcp.Server 
import qualified Network.Socket.ByteString.Lazy as S
import qualified Network.Socket as S hiding (recv)
import           Control.Monad.Extra

type Handler protocol  = Value -> D.Connection protocol -> IO ()
type Handlers protocol = Map Text (Handler protocol)
type ServerHandle      = TChan D.ServerComand

timeoutDelay = 5000

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan D.StopServer

tryTakeResponce :: Int -> MVar Bool -> IO (Either D.NetworkError ())
tryTakeResponce time feedback = do
    res <- timeout time False (takeMVar feedback)
    case res of
        Right b | b -> pure $ Right ()
        _           -> pure $ Left D.ConnectClosed

sendWithTimeOut conn msg = do
    feedback <- newEmptyMVar
    atomically $ do
        isEmpty <- isEmptyTMVar conn
        unless isEmpty $ do
            chan <- readTMVar conn
            writeTChan chan $ D.Send msg feedback
    tryTakeResponce timeoutDelay feedback

class NetworkConnection protocol where
    startServer :: PortNumber -> Handlers protocol -> (D.Connection protocol -> D.ConnectionVar protocol -> IO ()) -> (Text -> IO ()) -> IO ServerHandle
    -- | Send msg to node.
    send        :: D.ConnectionVar protocol -> LByteString -> IO (Either D.NetworkError ())
    close       :: D.ConnectionVar protocol -> STM ()
    openConnect :: D.Address -> Handlers protocol -> (Text -> IO ()) -> IO (D.ConnectionVar protocol)

