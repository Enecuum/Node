{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.Logger.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types as T

-- This is a raw vision of the logging language.

-- | Logging possibilities.
data LoggerF next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> Text -> (() -> next) -> LoggerF next

instance Functor LoggerF where
  fmap g (LogMessage level msg next) = LogMessage level msg (g . next)

type LoggerL next = Free LoggerF next

class Logger m where
  logMessage :: T.LogLevel -> Text -> m ()

instance Logger (Free LoggerF) where
  logMessage level msg = liftF $ LogMessage level msg id

-- | Log message with Info level.
logInfo :: Logger m => Text -> m ()
logInfo = logMessage T.Info

