{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A

import           Enecuum.Framework.Node.Language          ( NodeL )
import           Enecuum.Framework.Domain.RPC
import qualified Data.Text as T
import           Data.Typeable

-- | Rpc server description language.
data RpcMethodF  a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod  -> (() -> a)  -> RpcMethodF  a

instance Functor RpcMethodF where
  fmap g (RpcMethod text f next) = RpcMethod text f (g . next)

type RpcMethod  = A.Value -> Int -> NodeL  RpcResponse
type RpcMethodL  a = Free RpcMethodF a

rpcMethod :: Text -> RpcMethod -> RpcMethodL ()
rpcMethod text f = liftF (RpcMethod text f id)

makeMethod :: (FromJSON a, ToJSON b) => (a -> NodeL b) -> RpcMethod 
makeMethod f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        pure $ RpcResponseResult (A.toJSON res) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i

makeMethod' :: (FromJSON a, ToJSON b) => (a -> NodeL (Either Text b)) -> RpcMethod 
makeMethod' f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        case res of
            Right b -> pure $ RpcResponseResult (A.toJSON b) i
            Left  t -> pure $ RpcResponseError  (A.toJSON $ A.String t) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i

method
    :: (Typeable a, Typeable b, ToJSON b, FromJSON a) => (a -> NodeL b) -> RpcMethodL ()
method f = rpcMethod (makeMethodName f) (makeMethod f)

methodE
    :: (Typeable a, Typeable b, ToJSON b, FromJSON a) => (a -> NodeL (Either Text b)) -> RpcMethodL ()
methodE f = rpcMethod (makeMethodName f) (makeMethod' f)

makeMethodName :: Typeable a => a -> Text
makeMethodName = T.pack . takeWhile (/= ' ') . show . typeOf
