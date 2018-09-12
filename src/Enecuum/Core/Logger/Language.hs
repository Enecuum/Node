{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.Logger.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types as T

-- This is a raw vision of the logging language.

-- | Logging possibilities.
data LoggerL a where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> Text -> LoggerL ()

makeFreer ''LoggerL

-- | Log message with Info level.
logInfo
  :: ( Member LoggerL effs )
  => Text
  -> Eff effs ()
logInfo = logMessage T.Info
