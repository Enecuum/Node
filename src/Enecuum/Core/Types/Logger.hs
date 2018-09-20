{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Core.Types.Logger where

import           Enecuum.Prelude
import           Data.Aeson.Extra (noLensPrefix)

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

-- | Logging format.
type Format = String

data LoggerConfig = LoggerConfig
  { _currentFormat :: Format
  , _currentLevel  :: LogLevel
  , _logFilePath   :: FilePath
  } deriving (Generic, Show, Read)

instance ToJSON LoggerConfig where toJSON = genericToJSON noLensPrefix
instance FromJSON LoggerConfig where parseJSON = genericParseJSON noLensPrefix

standartFormat :: String
standartFormat = "$prio $loggername: $msg"

nullFormat :: String
nullFormat = "$msg"
