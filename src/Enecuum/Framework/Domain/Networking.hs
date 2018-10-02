{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Control.Concurrent.STM.TChan (TChan)
import           Network.Socket
import           Enecuum.Legacy.Refact.Network.Server
import           Data.Aeson

data NetworkConnection where
  NetworkConnection :: TMVar (TChan Comand) -> NetworkConnection

data Comand where
  Close       :: Comand
  Send        :: LByteString -> Comand

newtype ServerHandle = ServerHandle (TChan ServerComand)

-- | Node address (like IP)
data Address = Address
  { host :: String
  , port :: PortNumber
  } deriving (Show, Eq, Ord, Generic)

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port
