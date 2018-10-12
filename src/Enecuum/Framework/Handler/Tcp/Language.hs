{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.Handler.Tcp.Language where

import           Enecuum.Prelude
import           Data.Aeson as A
import           Enecuum.Framework.Domain.Tags as D
import           Data.Typeable
import qualified Enecuum.Framework.Domain             as D

-- | Rpc server description language.
data TcpHandlerF m a where
    -- | Set rpc method to list.
    TcpHandler :: Text -> TcpHandler m -> (() -> a)  -> TcpHandlerF m a

instance Functor (TcpHandlerF m) where
    fmap g (TcpHandler text f next) = TcpHandler text f (g . next)

type TcpHandler m  = A.Value -> D.Connection D.Tcp -> m ()
type TcpHandlerL m a = Free (TcpHandlerF m) a

msgHandler :: Text -> TcpHandler m -> TcpHandlerL m ()
msgHandler text f = liftF (TcpHandler text f id)

makeHandler :: (FromJSON a, Monad m) => (a -> D.Connection D.Tcp -> m ()) -> TcpHandler m
makeHandler f raw = case A.fromJSON raw of
    A.Success req -> \conn -> f req conn
    A.Error   _   -> \_ -> pure ()

handler :: (Typeable a, FromJSON a, Typeable m, Monad m) => (a -> D.Connection D.Tcp -> m ()) -> TcpHandlerL m ()
handler f = msgHandler (D.toTag f) (makeHandler f)
