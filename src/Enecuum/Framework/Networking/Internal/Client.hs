module Enecuum.Framework.Networking.Internal.Client (
      runClient
    ) where

import           Network.Socket
import           Enecuum.Domain as D 
import           Enecuum.Prelude

-- | Run client.
runClient :: SocketType -> D.Address -> (Socket -> IO ()) -> IO ()
runClient connectType address handler = do
    connection <- openConnect connectType address
    finally (handler connection) (close connection)

openConnect :: SocketType -> D.Address -> IO Socket
openConnect connectType (D.Address host port) = do
    address <- head <$> getAddrInfo Nothing (Just host) (Just $ show port)
    sock    <- socket (addrFamily address) connectType defaultProtocol
    connect sock $ addrAddress address
    pure sock
 