{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.MsgHandler.Language where

import           Enecuum.Prelude
import           Data.Aeson as A


import qualified Data.Text as T
import           Data.Typeable
import qualified Enecuum.Framework.Domain             as D

-- | Rpc server description language.
data MsgHandlerF m a where
  -- | Set rpc method to list.
  MsgHandler :: Text -> MsgHandler m -> (() -> a)  -> MsgHandlerF m a

instance Functor (MsgHandlerF m) where
  fmap g (MsgHandler text f next) = MsgHandler text f (g . next)

type MsgHandler m  = A.Value -> D.NetworkConnection -> m ()
type MsgHandlerL m a = Free (MsgHandlerF m) a

msgHandler :: Text -> MsgHandler m -> MsgHandlerL m ()
msgHandler text f = liftF (MsgHandler text f id)

makeHandler :: (FromJSON a, Monad m) => (a -> D.NetworkConnection -> m ()) -> MsgHandler m
makeHandler f raw = case A.fromJSON raw of
    A.Success req -> \conn -> f req conn
    A.Error   _   -> \_ -> pure ()


handler :: (Typeable a, FromJSON a, Typeable m, Monad m) => (a -> D.NetworkConnection -> m ()) -> MsgHandlerL m ()
handler f = msgHandler (makeTagName f) (makeHandler f)


makeTagName :: Typeable a => a -> Text
makeTagName = T.pack . takeWhile (/= ' ') . show . typeOf
