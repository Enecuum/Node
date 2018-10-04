{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A


import           Enecuum.Framework.Domain.RPC
import qualified Data.Text as T
import           Data.Typeable

-- | Rpc server description language.
data RpcMethodF m a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod m -> (() -> a)  -> RpcMethodF m a

instance Functor (RpcMethodF m) where
  fmap g (RpcMethod text f next) = RpcMethod text f (g . next)

type RpcMethod m  = A.Value -> Int -> m RpcResponse
type RpcMethodL m a = Free (RpcMethodF m) a

rpcMethod :: Text -> RpcMethod m -> RpcMethodL m ()
rpcMethod text f = liftF (RpcMethod text f id)

makeMethod :: (FromJSON a, ToJSON b, Monad m) => (a -> m b) -> RpcMethod m
makeMethod f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        pure $ RpcResponseResult (A.toJSON res) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i

makeMethod' :: (FromJSON a, ToJSON b, Monad m) => (a -> m (Either Text b)) -> RpcMethod m
makeMethod' f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        case res of
            Right b -> pure $ RpcResponseResult (A.toJSON b) i
            Left  t -> pure $ RpcResponseError  (A.toJSON $ A.String t) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i

method
    :: (Typeable a, Typeable b, ToJSON b, FromJSON a, Typeable m, Monad m) => (a -> m b) -> RpcMethodL m ()
method f = rpcMethod (makeMethodName f) (makeMethod f)

methodE
    :: (Typeable a, Typeable b, ToJSON b, FromJSON a, Typeable m, Monad m) => (a -> m (Either Text b)) -> RpcMethodL m ()
methodE f = rpcMethod (makeMethodName f) (makeMethod' f)

makeMethodName :: Typeable a => a -> Text
makeMethodName = T.pack . takeWhile (/= ' ') . show . typeOf
