{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

module Enecuum.Framework.Handler.Cmd.Language
  ( CmdHandlerF (..)
  , stdHandler
  , CmdHandler
  , CmdHandlerL
  , CLICommand
  ) where

import           Enecuum.Framework.Domain.Tags   as D
import qualified Enecuum.Framework.Node.Language as L
import           Enecuum.Prelude

data CmdHandlerF a where
    CmdHandler :: Text -> CmdHandler -> (() -> a)  -> CmdHandlerF a

instance Functor CmdHandlerF where
    fmap g (CmdHandler text f next) = CmdHandler text f (g . next)

type CmdHandler    = String -> L.NodeL Text
type CmdHandlerL a = Free CmdHandlerF a

stdHandler :: (Typeable a, Read a) => (a -> L.NodeL Text) -> CmdHandlerL ()
stdHandler f = liftF $ CmdHandler (D.toTag f) (makeStdHandler f) id

makeStdHandler :: Read a => (a -> L.NodeL Text) -> String -> L.NodeL Text
makeStdHandler f raw = case readMaybe raw of
    Just req -> f req
    Nothing  -> pure "Error of request parsing"

type CLICommand = String
