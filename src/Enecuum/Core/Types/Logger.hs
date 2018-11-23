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

standardFormat :: String
standardFormat = "$prio $loggername: $msg"

nullFormat :: String
nullFormat = "$msg"

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _format = standardFormat
    , _level = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    }