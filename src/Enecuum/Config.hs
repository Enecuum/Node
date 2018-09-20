{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Config where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as L
import qualified Data.Aeson                    as A

-- Dummy config
data Config = Config
  { bootNodeAddress :: Text
  , bootNode :: Bool
  , networkNode :: Bool
  , extPort :: Int 
  }
  deriving (Generic, FromJSON)

withConfig :: String -> (Config -> IO ()) -> IO ()
withConfig configName act = do
  configContents <- L.readFile configName
  case A.decode configContents of
    Nothing     -> error "Please, specify config file correctly"
    Just config -> act config
