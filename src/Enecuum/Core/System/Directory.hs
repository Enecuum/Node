module Enecuum.Core.System.Directory where

import           Enecuum.Prelude
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath  ((</>))


getEnecuumDir :: IO FilePath
getEnecuumDir = do
    enecuumDir <- liftM (</> "enecuum") getHomeDirectory
    createDirectoryIfMissing True enecuumDir
    pure enecuumDir

logFilePath :: IO FilePath
logFilePath = liftM (</> "data" </> "logs") getEnecuumDir


appFilename :: FilePath
appFilename = "app.log"
