module Enecuum.Core.Logger.Config where

import           Enecuum.Prelude

import           Data.Aeson
import qualified Data.ByteString.Lazy            as L
import           Enecuum.Assets.System.Directory (configFilePath)
import qualified Enecuum.Core.Types              as T

logConfig :: IO T.LoggerConfig
logConfig = do
  enc <- L.readFile =<< configFilePath
  case decode enc :: Maybe T.LoggerConfig of
      Nothing   -> error "Please, specify config file correctly"
      Just conf -> pure conf
