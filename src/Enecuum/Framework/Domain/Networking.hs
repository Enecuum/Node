{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.ByteString.Lazy          as BS
import           Enecuum.Legacy.Service.Network.Base (ConnectInfo(..))
import qualified Enecuum.Framework.Domain.RpcMessages as R
import Enecuum.Framework.Environment

-- Raw vision of networking api. Can change significantly.

data family NetworkConnection config
data instance NetworkConnection RealWorld =
    RealWorldNetworkConnection (TChan ConnectionComand)
  deriving (Typeable)

data instance NetworkConnection TestWorld =
    TestWorldNetworkConnection Connection
  deriving (Typeable)

data ConnectionComand
    = CloseConnection
    | SendRequest R.RpcRequest (TMVar (Either Text R.RpcResponse))

type RawData = BS.ByteString

-- | Node address (like IP)

type NodeAddress = ConnectInfo

infoToText :: ConnectInfo -> Text
infoToText (ConnectInfo a b) = T.pack a <> ":" <> show b

deriving instance Eq ConnectInfo
deriving instance Ord ConnectInfo

-- | Connection options.
data ConnectionConfig = ConnectionConfig
  { _address :: NodeAddress
  }

-- | Connection object. States that there is an open (maybe died) connection between nodes.
data Connection = Connection
  { _clientAddress :: NodeAddress
  , _serverAddress :: NodeAddress
  }
