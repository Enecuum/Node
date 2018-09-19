{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Core.Types.Logger where

import           Data.Aeson.TH
-- import           GHC.Generics  (Generic)
import           Prelude


-- | Logging level.
data LogLevel = Debug | Info | Warning | Error deriving (Eq, Ord, Show, Read)

-- | Logging format.
type Format = String

standartFormat :: String
standartFormat = "$prio $loggername: $msg"

nullFormat :: String
nullFormat = "$msg"


data LoggerConfig = LoggerConfig {
    _currentFormat :: Format
  , _currentLevel  :: LogLevel
  , _logFilePath   :: FilePath
  } deriving (Show, Read)

$(deriveJSON defaultOptions ''LogLevel)
$(deriveJSON defaultOptions ''LoggerConfig)
