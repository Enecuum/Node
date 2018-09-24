{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A

import           Enecuum.Framework.Node.Language          ( NodeL )
import           Enecuum.Framework.Domain.RpcMessages
import qualified Data.Text as T
import           Data.Typeable

-- | Rpc server description language.
data RpcMethodF cfg a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod cfg -> (() -> a)  -> RpcMethodF cfg a

instance Functor (RpcMethodF cfg) where
  fmap g (RpcMethod text method next) = RpcMethod text method (g . next)

type RpcMethod cfg = A.Value -> Int -> NodeL cfg RpcResponse
type RpcMethodL cfg a = Free (RpcMethodF cfg) a

rpcMethod :: Text -> RpcMethod cfg -> RpcMethodL cfg ()
rpcMethod text method = liftF (RpcMethod text method id)


makeMethod :: (FromJSON a, ToJSON b) => (a -> NodeL cfg b) -> RpcMethod cfg
makeMethod f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        pure $ RpcResponseResult (A.toJSON res) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i


makeMethod' :: (FromJSON a, ToJSON b) => (a -> NodeL cfg (Either Text b)) -> RpcMethod cfg
makeMethod' f a i = case A.fromJSON a of
    A.Success req -> do
        res <- f req
        case res of
            Right b -> pure $ RpcResponseResult (A.toJSON b) i
            Left  t -> pure $ RpcResponseError  (A.toJSON $ A.String t) i
    A.Error _     -> pure $ RpcResponseError  (A.toJSON $ A.String "Error in parsing of args") i


class MethodMaker cfg a | a -> cfg where
    method :: a -> RpcMethodL cfg ()

instance (Typeable a, Typeable cfg, Typeable b, ToJSON b, FromJSON a) =>
    MethodMaker cfg (a -> NodeL cfg b) where
        method f = rpcMethod (makeMethodName f) (makeMethod f)


makeMethodName :: Typeable a => a -> Text
makeMethodName = T.pack . takeWhile (/= ' ') . show . typeOf
