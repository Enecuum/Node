{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A

import           Enecuum.Framework.Node.Language          ( NodeModel )
import           Enecuum.Framework.Domain.RpcMessages

-- | Rpc server description language.
data RpcMethodL a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod -> (() -> a)  -> RpcMethodL a

instance Functor RpcMethodL where
  fmap g (RpcMethod text method next) = RpcMethod text method (g . next)

type RpcMethod = A.Value -> Int -> NodeModel RpcResponse

rpcMethod :: Text -> RpcMethod -> Free RpcMethodL ()
rpcMethod text method = liftF (RpcMethod text method id)
