{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Logger.Language where

import           Enecuum.Prelude

import           Control.Monad.Free (Free (..), liftF)
import qualified Enecuum.Core.Types as T

type MesType = Text

data LogMessageSimple = LogMessageSimple T.LogLevel Text


data LoggerF next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> Text -> (() -> next) -> LoggerF next
  -- | Setup config and Log message with a predefined level.
  SetupFile :: T.LogLevel -> FilePath -> T.Format -> (() -> next) -> LoggerF next  -- -> MesType


instance Functor LoggerF where
  fmap g (LogMessage level msg next) = LogMessage level msg (g . next)
  fmap g (SetupFile fileLevel filePath format  next) = -- txt
    SetupFile fileLevel filePath format (g . next) -- txt

type LoggerL next = Free LoggerF next

class Logger m where
  logMessage :: T.LogLevel -> MesType -> m ()
  setupFile :: T.LogLevel -> FilePath -> T.Format -> m () -- -> MesType
instance Logger (Free LoggerF) where
  logMessage level msg = liftF $ LogMessage level msg id
  setupFile level filePath format = liftF $ SetupFile level filePath format id -- txt


-- | Log message with Info level.
logInfo :: Logger m => MesType -> m ()
logInfo = logMessage T.Info
