{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as BS

-- Raw vision of networking api. Can change significantly.

type RawData = BS.ByteString

type NodeAddress = Text
data ConnectionConfig = ConnectionConfig
  { _address :: NodeAddress
  }

data Connection = Connection
  { _clientAddress :: NodeAddress
  , _serverAddress :: NodeAddress
  }

-- Temporary approach untill we clarify all the networking details.

data RpcRequest = RpcRequest
  { _rawData :: RawData
  }

data RpcResponse = RpcResponse
  { _rawData :: RawData
  }

type RpcResult a = Either Text a

-- TODO: remove double encode / decode from serving code.
class RpcMethod cfg req resp | req -> resp, resp -> req where
  toRpcRequest :: cfg -> req -> RpcRequest
  fromRpcResponse :: cfg -> RpcResponse -> RpcResult resp
