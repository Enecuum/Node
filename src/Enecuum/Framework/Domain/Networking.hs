{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveAnyClass         #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import qualified Data.Aeson as A
import           Control.Concurrent.STM.TChan (TChan)
import           Network.Socket
import           Enecuum.Legacy.Refact.Network.Server

data NetworkConnection = NetworkConnection
  { _address :: Address
  }
  deriving (Show, Eq, Ord, Generic)

type RawData = LByteString

data Comand where
  Close       :: Comand
  Send        :: RawData -> Comand

newtype ServerHandle = ServerHandle (TChan ServerComand)

data NetworkMsg = NetworkMsg Text A.Value deriving (Generic, ToJSON, FromJSON)

type Host = String

-- | Node address (like IP)
data Address = Address
  { _host :: Host
  , _port :: PortNumber
  } deriving (Show, Eq, Ord, Generic)

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port
