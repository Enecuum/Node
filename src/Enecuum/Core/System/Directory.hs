module Enecuum.Core.System.Directory where

import           Enecuum.Prelude
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath  ((</>))


getEnecuumDir :: IO FilePath
getEnecuumDir = createFilePath =<< liftM (</> "enecuum") getHomeDirectory

createFilePath :: FilePath -> IO FilePath
createFilePath file = do
  createDirectoryIfMissing True file
  pure file

logFilePath :: IO FilePath
logFilePath = createFilePath =<< liftM (</> "data" </> "logs") getEnecuumDir

appFileName :: IO FilePath
appFileName = liftM (</> "app.log") logFilePath

defaultLogFileName :: IO FilePath
defaultLogFileName = liftM (</> "default.log") logFilePath

configFilePath :: IO FilePath
configFilePath = do
  dir <- liftM (</> "configs" </> "Core" </> "Logger") getEnecuumDir
  createFilePath dir
  pure $ dir </> "config.json"
