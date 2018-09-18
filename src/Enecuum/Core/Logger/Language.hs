{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.Logger.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types as T


type MesType = Text



-- | Logging possibilities.
data LoggerL a where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> MesType -> LoggerL ()
  -- | Set config to handler
  SetConfigForLog ::  T.LogLevel -> FilePath -> T.Format -> LoggerL ()

-- This is a raw vision of the logging language.
makeFreer ''LoggerL

-- | Log message with Info level.
logInfo
  :: ( Member LoggerL effs )
  => MesType
  -> Eff effs ()
logInfo = logMessage T.Info
