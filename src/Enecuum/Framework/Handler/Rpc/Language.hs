{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.Handler.Rpc.Language where

import           Enecuum.Prelude
import           Data.Aeson as A
import           Control.Monad.Free
import           Enecuum.Framework.Domain.Tags as D
import           Enecuum.Framework.Domain.RPC

-- | Rpc server description language.
data RpcHandlerF m a where
    -- | Set rpc method to list.
    RpcHandler :: Text -> RpcHandler m -> (() -> a)  -> RpcHandlerF m a

instance Functor (RpcHandlerF m) where
    fmap g (RpcHandler text f next) = RpcHandler text f (g . next)

type RpcHandler  m   = A.Value -> Int -> m RpcResponse
type RpcHandlerL m a = Free (RpcHandlerF m) a

whenSucces
    :: (Applicative f, FromJSON t)
    => Value -> Int -> (t -> f RpcResponse) -> f RpcResponse
whenSucces a i f = case A.fromJSON a of
    A.Success req -> f req
    A.Error    _  -> 
        pure $ RpcResponseError (A.toJSON $ A.String "Error in parsing of args") i

makeRpc :: MonadFree (RpcHandlerF m1) m2 => Text -> RpcHandler m1 -> m2 ()
makeRpc t f = liftF $ RpcHandler t f id

makeMethod :: (FromJSON a, ToJSON b, Monad m) => (a -> m b) -> RpcHandler m
makeMethod f a i = whenSucces a i $ \req -> do
    res <- f req
    pure $ RpcResponseResult (A.toJSON res) i

makeMethod' :: (FromJSON a, ToJSON b, Monad m) => (a -> m (Either Text b)) -> RpcHandler m
makeMethod' f a i = whenSucces a i $ \req -> do
    res <- f req
    pure $ case res of
        Right b -> RpcResponseResult (A.toJSON b)            i
        Left  t -> RpcResponseError  (A.toJSON $ A.String t) i

method :: (Typeable a, Typeable b, ToJSON b, FromJSON a, Typeable m, Monad m) => (a -> m b) -> RpcHandlerL m ()
method f = makeRpc (D.toTag f) (makeMethod f)

methodE
    :: (Typeable a, Typeable b, ToJSON b, FromJSON a, Typeable m, Monad m)
    => (a -> m (Either Text b))
    -> RpcHandlerL m ()
methodE f = makeRpc (D.toTag f) (makeMethod' f)
