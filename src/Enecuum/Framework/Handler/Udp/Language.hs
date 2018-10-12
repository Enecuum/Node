{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.Handler.Udp.Language where

import           Enecuum.Prelude
import           Data.Aeson as A
import           Enecuum.Framework.Domain.Tags as D
import qualified Enecuum.Framework.Domain             as D

-- | Rpc server description language.
data UdpHandlerF m a where
    -- | Set rpc method to list.
    UdpHandler :: Text -> UdpHandler m -> (() -> a)  -> UdpHandlerF m a


instance Functor (UdpHandlerF m) where
    fmap g (UdpHandler text f next) = UdpHandler text f (g . next)

type UdpHandler m  = A.Value -> D.Connection D.Udp -> m ()
type UdpHandlerL m a = Free (UdpHandlerF m) a

udpHandler
    :: (Typeable a, FromJSON a, Typeable m, Monad m)
    => (a -> D.Connection D.Udp -> m ())
    -> UdpHandlerL m ()
udpHandler f = liftF $ UdpHandler (D.toTag f) (makeHandler f) id

makeHandler :: (FromJSON a, Monad m) => (a -> D.Connection D.Udp -> m ()) -> UdpHandler m
makeHandler f raw = case A.fromJSON raw of
    A.Success req -> \conn -> f req conn
    A.Error   _   -> \_ -> pure ()