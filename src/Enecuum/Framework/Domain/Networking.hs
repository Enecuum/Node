{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Control.Concurrent.STM.TChan (TChan)
import           Network.Socket
import           Enecuum.Legacy.Refact.Network.Server

data NetworkConnection = NetworkConnection
  { _address :: Address
  }

data ConnectionImplementation = ConnectionImplementation (TMVar (TChan Comand))

type RawData = LByteString

data Comand where
  Close       :: Comand
  Send        :: RawData -> Comand

newtype ServerHandle = ServerHandle (TChan ServerComand)


-- | Node address (like IP)
data Address = Address
  { _host :: String
  , _port :: PortNumber
  } deriving (Show, Eq, Ord, Generic)

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port
