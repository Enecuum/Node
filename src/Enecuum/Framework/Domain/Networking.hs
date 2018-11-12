{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE BangPatterns           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

data NetworkError = ConnectionClosed | TooBigMessage | AddressNotExist deriving (Eq, Show)

newtype BoundAddress = BoundAddress Address
    deriving (Show, Eq, Ord, Generic)

newtype Connection a = Connection
    { _address :: BoundAddress
    }
    deriving (Show, Eq, Ord, Generic)

type CloseSignal = TVar Bool

class AsNativeConnection a where
    data family NativeConnection a
    isClosed :: NativeConnection a -> IO Bool

sockVarIsClosed sockVar = do
    S.MkSocket _ _ _ _ mStat <- atomically $ readTMVar sockVar
    status <- readMVar mStat
    pure $ status == S.Closed

instance AsNativeConnection Tcp where
    data NativeConnection Tcp
        = TcpConnection !(TMVar S.Socket) ThreadId BoundAddress
    isClosed (TcpConnection sockVar _ _) = sockVarIsClosed sockVar


instance AsNativeConnection Udp where
    data NativeConnection Udp
        = ServerUdpConnection !S.SockAddr  !(TMVar S.Socket)
        | ClientUdpConnection !CloseSignal !(TMVar S.Socket)

    isClosed (ServerUdpConnection _ sockVar) = sockVarIsClosed sockVar
    isClosed (ClientUdpConnection _ sockVar) = sockVarIsClosed sockVar

data ServerComand = StopServer

type RawData = LByteString

-- newtype ServerHandle = ServerHandle (TChan ServerComand)

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
    } deriving (Show, Eq, Ord, Generic)

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


-- | Tries to parse address of the form "0.0.0.0:0000."
-- It may throw exception or may parse the address incorectly.
unsafeParseAddress :: String -> Address
unsafeParseAddress strAddr | length strAddr < 3 || ':' `notElem` strAddr = error "Address parse error: malformed string."
unsafeParseAddress strAddr = let
    hostStr = takeWhile (/= ':') strAddr
    portStr = drop 1 $ dropWhile (/= ':') strAddr
    mbPort  = readMaybe portStr
    port    = fromMaybe (error "Address parse error: port is not a number") mbPort
    in Address hostStr port