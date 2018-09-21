{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A

import           Enecuum.Framework.Node.Language          ( NodeL )
import           Enecuum.Framework.Domain.RpcMessages
import qualified Data.Text as T
import           Data.Typeable

-- | Rpc server description language.
data RpcMethodL a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod -> (() -> a)  -> RpcMethodL a

instance Functor RpcMethodL where
  fmap g (RpcMethod text method next) = RpcMethod text method (g . next)

type RpcMethod = A.Value -> Int -> NodeL RpcResponse

rpcMethod :: Text -> RpcMethod -> Free RpcMethodL ()
rpcMethod text method = liftF (RpcMethod text method id)


makeMethod :: (FromJSON a, ToJSON b) => (a -> NodeL b) -> A.Value -> Int -> NodeL RpcResponse
makeMethod f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        pure $ RpcResponseResult (A.toJSON res) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i


makeMethod' :: (FromJSON a, ToJSON b) => (a -> NodeL (Either Text b)) -> A.Value -> Int -> NodeL RpcResponse
makeMethod' f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        case res of
            Right b -> pure $ RpcResponseResult (A.toJSON b) i
            Left  t -> pure $ RpcResponseError  (A.toJSON $ A.String t) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i


class MethodMaker a where
    method :: a -> Free RpcMethodL ()

instance (Typeable a, Typeable b, ToJSON b, FromJSON a) => MethodMaker (a -> NodeL b) where
    method f = rpcMethod (makeMethodName f) (makeMethod f)


makeMethodName :: Typeable a => a -> Text
makeMethodName = T.pack . takeWhile (/= ' ') . show . typeOf
