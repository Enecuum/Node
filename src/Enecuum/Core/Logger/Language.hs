{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Logger.Language where

import qualified Enecuum.Core.Types as T (LogLevel (..), Message)
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

-- | Language for logging.
data LoggerF next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> T.Message -> (() -> next) -> LoggerF next

makeFunctorInstance ''LoggerF

type LoggerL next = Free LoggerF next

class Logger m where
  logMessage :: T.LogLevel -> T.Message -> m ()

instance Logger (Free LoggerF) where
  logMessage level msg = liftF $ LogMessage level msg id

-- | Log message with Info level.
logInfo :: Logger m => T.Message -> m ()
logInfo = logMessage T.Info

-- | Log message with Error level.
logError :: Logger m => T.Message -> m ()
logError = logMessage T.Error

-- | Log message with Debug level.
logDebug :: Logger m => T.Message -> m ()
logDebug = logMessage T.Debug

-- | Log message with Warning level.
logWarning :: Logger m => T.Message -> m ()
logWarning = logMessage T.Warning
