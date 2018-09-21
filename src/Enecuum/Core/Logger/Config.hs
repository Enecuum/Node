module Enecuum.Core.Logger.Config where

import           Enecuum.Prelude

import           Data.Aeson
import qualified Data.ByteString.Lazy            as L
import           Enecuum.Assets.System.Directory (configFilePath,
                                                  createFilePath)
import qualified Enecuum.Core.Types              as T
import           System.FilePath.Windows

logConfig :: IO T.LoggerConfig
logConfig = do
  enc <- L.readFile configFilePath
  conf <- case decode enc :: Maybe T.LoggerConfig of
      Nothing   -> error "Please, specify config file correctly"
      Just conf -> pure conf
  let (T.LoggerConfig _ _ logFile) = conf
  let dir = dropFileName logFile
  createFilePath dir
  pure conf
