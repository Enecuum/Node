{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude
import qualified Data.Text as T

import qualified Data.ByteString.Lazy          as BS
import           Enecuum.Legacy.Service.Network.Base (ConnectInfo(..))
import qualified Enecuum.Framework.Domain.RpcMessages as R

-- Raw vision of networking api. Can change significantly.

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

-- Temporary approach untill we clarify all the networking details.

-- | RPC request which wraps the RPC actions.
data RpcRequest = RpcRequest
  { _rawData :: R.RpcRequest
  }

-- | RPC response which wraps the RPC responses.
data RpcResponse = RpcResponse
  { _rawData :: R.RpcResponse
  }

-- | Result of RPC call.
type RpcResult a = Either Text a

-- TODO: remove double encode / decode from serving code.
-- | Describes how to convert a particular pair of request and response
-- to the RPC Request and Response.
-- Can take some config having some conversion options.
{-
class RpcMethod cfg req resp | req -> resp, resp -> req where
  toRpcRequest :: cfg -> req -> RpcRequest
  fromRpcResponse :: cfg -> RpcResponse -> RpcResult resp
-}