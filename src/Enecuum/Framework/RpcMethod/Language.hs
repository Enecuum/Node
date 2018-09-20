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


makeMethod :: (FromJSON a, ToJSON b) => (a -> NodeModel b) -> A.Value -> Int -> NodeModel RpcResponse
makeMethod f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        pure $ RpcResponseResult (A.toJSON res) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i


makeMethod' :: (FromJSON a, ToJSON b) => (a -> NodeModel (Either Text b)) -> A.Value -> Int -> NodeModel RpcResponse
makeMethod' f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        case res of
            Right b -> pure $ RpcResponseResult (A.toJSON b) i
            Left  t -> pure $ RpcResponseError  (A.toJSON $ A.String t) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i


class MethodMaker a where
    method :: Text -> a -> Free RpcMethodL ()

instance (ToJSON b, FromJSON a) => MethodMaker (a -> NodeModel b) where
    method t f = rpcMethod t (makeMethod f)

instance (ToJSON b, FromJSON a) => MethodMaker (a -> NodeModel (Either Text b)) where
    method t f = rpcMethod t (makeMethod' f)