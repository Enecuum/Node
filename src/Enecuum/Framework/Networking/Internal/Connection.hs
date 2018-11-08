module Enecuum.Framework.Networking.Internal.Connection where

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
type ServerHandle      = TChan D.ServerComand

-- | Stop the server
stopServer :: ServerHandle -> STM ()
stopServer chan = writeTChan chan D.StopServer


class NetworkConnection protocol where
    startServer
        :: S.PortNumber
        -> Handlers protocol
        -> (D.Connection protocol -> D.ConnectionVar protocol -> IO Bool)
        -> (Text -> IO ())
        -> IO ServerHandle
    -- | Send msg to node.
    send        :: D.ConnectionVar protocol -> LByteString -> IO (Either D.NetworkError ())
    close       :: D.ConnectionVar protocol -> STM ()
    openConnect :: D.Address -> Handlers protocol -> (Text -> IO ()) -> IO (Maybe (D.ConnectionVar protocol))
