{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.ByteString.Lazy          as BS
import qualified Enecuum.Framework.Domain.RPC as R
import           Enecuum.Framework.Domain.RPC
import           Network.Socket
import           Enecuum.Legacy.Refact.Network.Server
import           Data.Aeson

data NetworkConnection where
  NetworkConnection :: TMVar (TChan Comand) -> NetworkConnection

data Comand where
  Close       :: Comand
  Send        :: Value -> Comand

newtype ServerHandle = ServerHandle (TChan ServerComand)

-- | Node address (like IP)
data Address = Address
  { host :: String
  , port :: PortNumber
  } deriving (Show, Eq, Ord, Generic)

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port
