{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Core.Types.Logger where

import           Data.Aeson.Extra (noLensPrefix)
import           Enecuum.Prelude

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

-- | Logging format.
type Format = String

data LoggerConfig = LoggerConfig
  { _format       :: Format
  , _level        :: LogLevel
  , _logFilePath  :: FilePath
  , _logToConsole :: Bool
  , _logToFile    :: Bool
  } deriving (Generic, Show, Read)

instance ToJSON LoggerConfig where toJSON = genericToJSON noLensPrefix
instance FromJSON LoggerConfig where parseJSON = genericParseJSON noLensPrefix

type Message = Text

standartFormat :: String
standartFormat = "$prio $loggername: $msg"

nullFormat :: String
nullFormat = "$msg"
