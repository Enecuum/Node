{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.StdinHandlers.Language
  ( StdinHandlersF (..)
  , stdHandler
  , StdinHandler
  , StdinHandlerL
  ) where

import           Enecuum.Prelude
import           Data.Aeson as A

import qualified Data.Text as T
import           Data.Typeable
import qualified Enecuum.Framework.Node.Language      as L

data StdinHandlersF a where
  StdinHandler :: Text -> StdinHandler -> (() -> a)  -> StdinHandlersF a

instance Functor StdinHandlersF where
  fmap g (StdinHandler text f next) = StdinHandler text f (g . next)

type StdinHandler  = A.Value -> L.NodeL Text
type StdinHandlerL a = Free StdinHandlersF a

msgHandler :: Text -> StdinHandler -> StdinHandlerL ()
msgHandler text f = liftF (StdinHandler text f id)

makeStdHandler :: FromJSON a => (a -> L.NodeL Text) -> A.Value -> L.NodeL Text
makeStdHandler f raw = case A.fromJSON raw of
    A.Success req -> f req
    A.Error _     -> pure "Error of request parsing"

stdHandler  :: (Typeable a, FromJSON a) => (a -> L.NodeL Text) -> StdinHandlerL ()
stdHandler f = msgHandler (makeTagName f) (makeStdHandler f)

makeTagName :: Typeable a => a -> Text
makeTagName = T.pack . takeWhile (/= ' ') . show . typeOf
