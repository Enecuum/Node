{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeInType      #-}

module Enecuum.Framework.Handler.Network.Language where

import           Data.Aeson                    as A
import qualified Enecuum.Framework.Domain      as D
import           Enecuum.Prelude

-- | Rpc server description language.
data NetworkHandlerF p m a where
    -- | Set rpc method to list.
    NetworkHandler :: Text -> NetworkHandler p m -> (() -> a)  -> NetworkHandlerF p m a

instance Functor (NetworkHandlerF p m) where
    fmap g (NetworkHandler text f next) = NetworkHandler text f (g . next)

type NetworkHandler p m  = A.Value -> D.Connection p -> m ()
type NetworkHandlerL p m a = Free (NetworkHandlerF p m) a

msgHandler :: Text -> NetworkHandler p m -> NetworkHandlerL p m ()
msgHandler text f = liftF (NetworkHandler text f id)

makeHandler :: (FromJSON a, Monad m) => (a -> D.Connection p -> m ()) -> NetworkHandler p m
makeHandler f raw = case A.fromJSON raw of
    A.Success req -> f req
    A.Error   _   -> \_ -> pure ()

handler
    ::  forall k a m (p :: k).
        (Typeable a, Typeable k, FromJSON a, Typeable m, Monad m, Typeable p)
    => (a -> D.Connection p -> m ()) -> NetworkHandlerL p m ()
handler f = msgHandler (D.toTag f) (makeHandler f)