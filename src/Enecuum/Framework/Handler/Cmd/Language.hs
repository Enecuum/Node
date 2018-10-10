{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.Handler.Cmd.Language
  ( CmdHandlerF (..)
  , stdHandler
  , CmdHandler
  , CmdHandlerL
  ) where

import           Enecuum.Prelude
import           Data.Aeson as A

import           Enecuum.Framework.Domain.Tags as D
import           Data.Typeable
import qualified Enecuum.Framework.Node.Language      as L

data CmdHandlerF a where
    CmdHandler :: Text -> CmdHandler -> (() -> a)  -> CmdHandlerF a

instance Functor CmdHandlerF where
    fmap g (CmdHandler text f next) = CmdHandler text f (g . next)

type CmdHandler  = A.Value -> L.NodeL Text
type CmdHandlerL a = Free CmdHandlerF a

stdHandler :: (Typeable a, FromJSON a) => (a -> L.NodeL Text) -> CmdHandlerL ()
stdHandler f = liftF $ CmdHandler (D.toTag f) (makeStdHandler f) id

makeStdHandler :: FromJSON a => (a -> L.NodeL Text) -> A.Value -> L.NodeL Text
makeStdHandler f raw = case A.fromJSON raw of
    A.Success req -> f req
    A.Error   _   -> pure "Error of request parsing"