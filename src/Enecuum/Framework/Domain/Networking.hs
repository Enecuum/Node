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

data Udp
data Tcp

data Connection a = Connection
    { _address :: Address
    }
    deriving (Show, Eq, Ord, Generic)

data family ConnectionVar a
data instance ConnectionVar Tcp
    = TcpConnectionVar (TMVar (TChan Comand))

data instance ConnectionVar Udp
    = ServerUdpConnectionVar S.SockAddr (TChan SendMsg)
    | ClientUdpConnectionVar (TMVar (TChan Comand))

data Protocol     = UDP | TCP
data ServerComand = StopServer

type RawData = LByteString

data SendMsg = SendMsg SockAddr LByteString

data Comand where
    Close :: Comand
    Send  :: RawData -> Comand

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
