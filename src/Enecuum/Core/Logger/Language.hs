{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.Logger.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types as T

data LoggerL a where
  LogMessage :: T.LogLevel -> Text -> LoggerL ()

makeFreer ''LoggerL

logInfo
  :: ( Member LoggerL effs )
  => Text
  -> Eff effs ()
logInfo = logMessage T.Info
