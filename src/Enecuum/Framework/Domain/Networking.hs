{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as BS

-- Raw vision of networking api. Can change significantly.

type RawData = BS.ByteString

data ConnectionConfig = ConnectionConfig
data Connection = Connection

-- Temporary approach untill we clarify all the networking details.

data RpcRequest = RpcRequest
  { _rawData :: RawData
  }

data RpcResponse = RpcResponse
  { _rawData :: RawData
  }

type RpcResult a = Either Text a

class RpcMethod cfg req resp | req -> resp, resp -> req where
  toRpcRequest :: cfg -> req -> RpcRequest
  fromRpcResponse :: cfg -> RpcResponse -> Maybe resp
