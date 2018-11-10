module Enecuum.Framework.Networking.Internal.Connection where

import           Control.Concurrent (killThread)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Data.Aeson
import           Enecuum.Prelude
import qualified Enecuum.Framework.Domain.Networking as D
import           Control.Monad.Extra
import qualified Network.Socket                      as S hiding (recv)
import qualified Data.IP as IP

type Handler protocol  = Value -> D.Connection protocol -> IO ()
type Handlers protocol = Map Text (Handler protocol)
data ServerHandle
      = ServerHandle (TMVar S.Socket) ThreadId
      | OldServerHandle (TChan D.ServerComand)

-- | Stop the server
stopServer :: ServerHandle -> IO ()
stopServer (OldServerHandle chan) = atomically $ writeTChan chan D.StopServer
stopServer (ServerHandle sockVar acceptWorkerId) = do
    sock <- trace "[stopServer] Taking listener sock" $ atomically $ takeTMVar sockVar
    manualCloseConnection' sock acceptWorkerId
    trace "[stopServer] Releasing listener sock" $ atomically (putTMVar sockVar sock)

type Connections protocol = (D.Connection protocol, D.NativeConnection protocol)

class NetworkConnection protocol where
    startServer
        :: S.PortNumber
        -> Handlers protocol
        -> (D.Connection protocol -> D.NativeConnection protocol -> IO Bool)
        -> IO (Maybe ServerHandle)
    -- | Send msg to node.
    send  :: D.NativeConnection protocol -> LByteString -> IO (Either D.NetworkError ())
    open  :: D.Address -> Handlers protocol -> IO (Maybe (Connections protocol))
    close :: D.NativeConnection protocol -> IO ()


manualCloseConnection connVar@(D.TcpConnection sockVar readerId boundAddr) = do
    sock <- trace ("[manualCloseConnection] " <> show boundAddr <> " Taking sock") $ atomically $ takeTMVar sockVar
    manualCloseConnection' sock readerId
    trace ("[manualCloseConnection] " <> show boundAddr <> " Releasing sock") $ atomically (putTMVar sockVar sock)

manualCloseConnection' sock readerId = do
    trace "[manualCloseConnection] killing thread" $ killThread readerId
    manualCloseSock sock
    trace_ "[manualCloseConnection] done"

manualCloseSock sock = do
    trace_ "[manualCloseSock] closing sock"
    eRes <- try $ S.close sock
    whenLeft eRes $ \(err :: SomeException) -> trace_ $ "[manualCloseSock] exc got in closing sock: " <> show err


fromSockAddr :: S.SockAddr -> D.Address
fromSockAddr (S.SockAddrInet port host)       = D.Address (show $ IP.fromHostAddress host)   port
fromSockAddr (S.SockAddrInet6 port _ host6 _) = D.Address (show $ IP.fromHostAddress6 host6) port
fromSockAddr (S.SockAddrUnix str)             = D.unsafeParseAddress str
fromSockAddr sa                               = error $ "fromSockAddr not supported for: " <> show sa
