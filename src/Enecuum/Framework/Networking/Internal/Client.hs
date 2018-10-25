module Enecuum.Framework.Networking.Internal.Client (
      runClient
    , Protocol(..)
    ) where

import           Network.Socket
import           Enecuum.Domain as D 
import           Enecuum.Prelude

-- | Run client.
runClient :: D.Protocol a -> D.Address -> (Socket -> IO ()) -> IO ()
runClient protocol address handler = do
    connection <- openConnect protocol address          -- TODO: handle exception somehow.
    finally (handler connection) (close connection)

openConnect :: D.Protocol a -> D.Address -> IO Socket
openConnect protocol (D.Address host port) = do
    let connectType = case protocol of
            D.TCP -> Stream
            D.UDP -> Datagram

    address <- head <$> getAddrInfo Nothing (Just host) (Just $ show port)
    sock    <- socket (addrFamily address) connectType defaultProtocol
    connect sock $ addrAddress address
    pure sock
 