{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Config where

import qualified Data.ByteString.Lazy          as L
import           Data.Text                     (Text)
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( FromJSON )
import           GHC.Generics                             ( Generic )

-- Dummy config
data Config = Config
  { bootNodeAddress :: Text

  }
  deriving (Generic, FromJSON)

withConfig :: String -> (Config -> IO ()) -> IO ()
withConfig configName act = do
  configContents <- L.readFile configName
  case A.decode configContents of
    Nothing     -> error "Please, specify config file correctly"
    Just config -> act config
