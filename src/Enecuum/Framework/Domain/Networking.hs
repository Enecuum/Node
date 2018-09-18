{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as BS
import qualified Data.Aeson                    as A


-- Raw vision of networking api. Can change significantly.

type RawData = BS.ByteString

-- | Node address (like IP)

type NodeAddress = Text


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
  { _rawData :: RawData
  }

-- | RPC response which wraps the RPC responses.
data RpcResponse = RpcResponse
  { _rawData :: RawData
  }

-- | Result of RPC call.
type RpcResult a = Either Text a

-- TODO: remove double encode / decode from serving code.
-- | Describes how to convert a particular pair of request and response
-- to the RPC Request and Response.
-- Can take some config having some conversion options.
class RpcMethod cfg req resp | req -> resp, resp -> req where
  toRpcRequest :: cfg -> req -> RpcRequest
  fromRpcResponse :: cfg -> RpcResponse -> RpcResult resp
