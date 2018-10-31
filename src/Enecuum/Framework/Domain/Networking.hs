{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Data.Scientific
import           Data.IP
import qualified Data.Aeson as A
import           Control.Concurrent.STM.TChan (TChan)
import           Network.Socket
import qualified Network.Socket as S hiding (recv)


data Udp = Udp
data Tcp = Tcp
data Rpc = Rpc

data NetworkError = ConnectionClosed | TooBigMessage | AddressNotExist deriving Eq

newtype Connection a = Connection
    { _address :: Address
    }
    deriving (Show, Eq, Ord, Generic)

data family ConnectionVar a
data instance ConnectionVar Tcp
    = TcpConnectionVar (TMVar (TChan Command))

data instance ConnectionVar Udp
    = ServerUdpConnectionVar S.SockAddr (TChan SendUdpMsgTo)
    | ClientUdpConnectionVar (TMVar (TChan Command))

data ServerComand = StopServer

type RawData = LByteString

data SendUdpMsgTo = SendUdpMsgTo SockAddr LByteString (MVar Bool)

data Command where
    Close :: Command
    Send  :: RawData -> MVar Bool -> Command

newtype ServerHandle = ServerHandle (TChan ServerComand)

data NetworkMsg = NetworkMsg Text A.Value deriving (Generic, ToJSON, FromJSON)

type Host = String

sockAddrToHost :: S.SockAddr -> Host
sockAddrToHost sockAddr = case sockAddr of
    S.SockAddrInet _ hostAddress      -> show $ fromHostAddress hostAddress
    S.SockAddrInet6 _ _ hostAddress _ -> show $ fromHostAddress6 hostAddress
    S.SockAddrUnix string             -> string
    _                                 -> error "Error"

-- | Node address (like IP)
data Address = Address
    { _host :: Host
    , _port :: PortNumber
    } deriving (Show, Eq, Ord, Generic, Serialize)

deriving instance Generic PortNumber
instance Serialize PortNumber

instance ToJSON Address where
    toJSON (Address h p) = A.object ["host" A..= h, "port" A..= p]

instance FromJSON Address where
    parseJSON = A.withObject "Address" $ \v -> Address
        <$> v A..: "host"
        <*> v A..: "port"

instance ToJSON PortNumber where
    toJSON = toJSON.fromEnum

instance FromJSON PortNumber where
    parseJSON (A.Number a) = pure.toEnum.fromJust.toBoundedInteger $ a
    parseJSON _            = mzero

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port

packetSize :: Int
packetSize = 1024*4