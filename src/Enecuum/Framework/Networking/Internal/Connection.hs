module Enecuum.Framework.Networking.Internal.Connection where

import           Control.Concurrent (killThread)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Data.Aeson
-- import           Data.Aeson.Lens
import           Enecuum.Prelude
-- import           Control.Concurrent.Async
import qualified Enecuum.Framework.Domain.Networking as D
-- import           Enecuum.Framework.Networking.Internal.Client
-- import           Enecuum.Framework.Networking.Internal.Tcp.Server
import           Control.Monad.Extra
import qualified Network.Socket                      as S hiding (recv)
-- import qualified Network.Socket.ByteString.Lazy      as S

type Handler protocol  = Value -> D.Connection protocol -> IO ()
type Handlers protocol = Map Text (Handler protocol)
data ServerHandle
      = ServerHandle (TMVar S.Socket) ThreadId
      | OldServerHandle (TChan D.ServerComand)

-- | Stop the server
stopServer :: ServerHandle -> IO ()
stopServer (OldServerHandle chan) = atomically $ writeTChan chan D.StopServer
stopServer (ServerHandle sockVar acceptWorkerId) = do
    sock <- trace @String "[stopServer] Taking listener sock" $ atomically $ takeTMVar sockVar
    manualCloseConnection' sock acceptWorkerId
    trace @String "[stopServer] Releasing listener sock" $ atomically (putTMVar sockVar sock)

class NetworkConnection protocol where
    startServer
        :: S.PortNumber
        -> Handlers protocol
        -> (D.Connection protocol -> D.ConnectionVar protocol -> IO Bool)
        -> IO (Maybe ServerHandle)
    -- | Send msg to node.
    send        :: D.ConnectionVar protocol -> LByteString -> IO (Either D.NetworkError ())
    close       :: D.ConnectionVar protocol -> IO ()
    openConnect :: D.Address -> Handlers protocol -> IO (Maybe (D.ConnectionVar protocol))


manualCloseConnection connVar@(D.TcpConnectionVar sockVar readerId) = do
    sock <- trace @String "[manualCloseConnection] Taking sock" $ atomically $ takeTMVar sockVar
    manualCloseConnection' sock readerId
    trace @String "[manualCloseConnection] Releasing sock" $ atomically (putTMVar sockVar sock)

manualCloseConnection' sock readerId = do
    trace @String "[manualCloseConnection] killing thread" $ killThread readerId
    manualCloseSock sock
    trace @String "[manualCloseConnection] done" $ pure ()

manualCloseSock sock = do
    trace @String "[manualCloseSock] closing sock" $ pure ()
    eRes <- try $ S.close sock
    whenLeft eRes $ \(err :: SomeException) -> trace @String ("[manualCloseSock] exc got in closing sock: " <> show err) $ pure ()
