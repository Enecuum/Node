module Enecuum.Assets.System.Directory where

import qualified Enecuum.Core.Language as L
import           Enecuum.Prelude
-- import           System.Directory      (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath       ((</>))

getEnecuumDir :: (L.FileSystem m, Monad m) => m FilePath
getEnecuumDir = L.createFilePath =<< liftM (</> ".enecuum") L.getHomeDirectory
keysFilePath, logFilePath, storyFilePath, appFileName, clientStory, defaultLogFileName :: (L.FileSystem m, Monad m) => m FilePath
keysFilePath = liftM (</> "keys.txt") getEnecuumDir
logFilePath = L.createFilePath =<< liftM (</> "data" </> "logs") getEnecuumDir
storyFilePath = L.createFilePath =<< liftM (</> "story") getEnecuumDir
appFileName = L.createFilePath =<< liftM (</> "data" </> "logs" </> "app.log") getEnecuumDir
clientStory = liftM (</> "client.story") storyFilePath
defaultLogFileName = liftM (</> "default.log") logFilePath
configFilePath = "./configs/config.json"
