{-#Language TypeSynonymInstances, FlexibleInstances#-}
module Service.Network.UDP.Client (
    ClientHandle(..),
    runClient,
    PortNumber(..),
    HostAddress
  ) where

import Network.Socket
import Service.Network.Base

-- | Client handle data for your app.
data ClientHandle = ClientHandle {
    clientSocket  :: Socket,
    clientAddress :: SockAddr
  }

class Hosts a where
    openConnect :: a -> PortNumber -> IO ClientHandle


-- | Run a UDP client.
runClient :: Hosts a => a -> PortNumber -> (ClientHandle -> IO ()) -> IO ()
runClient aHostAdress aPort aPlainHandler = withSocketsDo $ do
    aHandle <- openConnect aHostAdress aPort
    aPlainHandler aHandle
    close (clientSocket aHandle)

instance Hosts HostAddress where
    openConnect aHostAdress = openConnect (showHostAddress aHostAdress)

instance Hosts String where
    openConnect aHostAdress aPort = do
        aServerAddr <- head <$> getAddrInfo
            Nothing
            (Just $ aHostAdress)
            (Just $ show aPort)
        aSocket <- socket (addrFamily aServerAddr) Datagram defaultProtocol
        return $ ClientHandle aSocket (addrAddress aServerAddr)
