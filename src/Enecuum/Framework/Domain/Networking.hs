{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Data.IP
import qualified Data.Aeson as A
import           Control.Concurrent.STM.TChan (TChan)
import           Network.Socket
import qualified Network.Socket as S hiding (recv)


data Udp = Udp
data Tcp = Tcp
data Rpc = Rpc

data NetworkError = ConnectClosed | TooBigMsg | NotExistAddress

data Protocol a = UDP | TCP

data Connection a = Connection
    { _address :: Address
    }
    deriving (Show, Eq, Ord, Generic)

data family ConnectionVar a
data instance ConnectionVar Tcp
    = TcpConnectionVar (TMVar (TChan Comand))

data instance ConnectionVar Udp
    = ServerUdpConnectionVar S.SockAddr (TChan SendUdpMsgTo)
    | ClientUdpConnectionVar (TMVar (TChan Comand))

data ServerComand = StopServer

type RawData = LByteString

data SendUdpMsgTo = SendUdpMsgTo SockAddr LByteString (MVar Bool)

data Comand where
    Close :: Comand
    Send  :: RawData -> MVar Bool -> Comand

newtype ServerHandle = ServerHandle (TChan ServerComand)

data NetworkMsg = NetworkMsg Text A.Value deriving (Generic, ToJSON, FromJSON)

type Host = String

sockAddrToHost :: S.SockAddr -> Host
sockAddrToHost sockAddr = case sockAddr of
    S.SockAddrInet _ hostAddress      -> show $ fromHostAddress hostAddress
    S.SockAddrInet6 _ _ hostAddress _ -> show $ fromHostAddress6 hostAddress
    S.SockAddrUnix string             -> string
    S.SockAddrCan  i                  -> show i

-- | Node address (like IP)
data Address = Address
    { _host :: Host
    , _port :: PortNumber
    } deriving (Show, Eq, Ord, Generic)

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port

packetSize :: Int
packetSize = 1024*4