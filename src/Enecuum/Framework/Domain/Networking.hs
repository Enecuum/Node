{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.ByteString.Lazy          as BS
import qualified Enecuum.Framework.Domain.RpcMessages as R
import           Enecuum.Framework.Domain.RpcMessages
import           Network.Socket

data NetworkConnection where
  NetworkConnection :: ConnectionClass b => b -> NetworkConnection

newtype RealConnection = RealConnection (TChan ConnectionComand)

data ConnectionComand
    = CloseConnection
    | SendRequest R.RpcRequest (TMVar (Either Text R.RpcResponse))

class ConnectionClass a where
  openConnection  :: Address -> IO (Maybe a)
  closeConnection :: a -> IO ()
  sendRequest     :: a -> RpcRequest -> IO (Either Text RpcResponse)

-- | Node address (like IP)
data Address = Address
  { host :: String
  , port :: PortNumber
  } deriving (Show, Eq, Ord, Generic)

formatAddress :: Address -> Text
formatAddress (Address addr port) = T.pack addr <> ":" <> show port

-- | Connection options.
-- data ConnectionConfig = ConnectionConfig
--   { _address :: Address
--   }

-- | Connection object. States that there is an open (maybe died) connection between nodes.
-- data Connection = Connection
--   { _clientAddress :: Address
--   , _serverAddress :: Address
--   }
