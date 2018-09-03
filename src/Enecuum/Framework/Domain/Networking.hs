{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.Domain.Networking where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types                      as T

-- Raw vision of networking api. Can change significantly.

data ConnectionConfig = ConnectionConfig
data Connection = Connection

data ServerDef = ServerDef
  {

  }

-- Temporary approach untill we clarify all the networking details.

data RpcRequest = RpcRequest
  {

  }

data RpcResponse = RpcResponse
  {
    
  }

type RpcResult a = Either Text a

class RpcMethod cfg req resp | req -> resp, resp -> req where
  toRpcRequest :: cfg -> req -> RpcRequest
  fromRpcResponse :: cfg -> RpcResponse -> Maybe resp

