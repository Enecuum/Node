module Enecuum.Assets.System.Directory where

import qualified Enecuum.Core.Language as L
import           Enecuum.Prelude
-- import           System.Directory      (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath       ((</>))

getEnecuumDir :: (L.FileSystem m, Monad m) => m FilePath
getEnecuumDir = L.createFilePath =<< (</> ".enecuum") <$> L.getHomeDirectory
keysFilePath, logFilePath, storyFilePath, appFileName, clientStory, defaultLogFileName, wrongKeysFilePath :: (L.FileSystem m, Monad m) => m FilePath
keysFilePath = (</> "keys.txt") <$> getEnecuumDir
wrongKeysFilePath = (</> "wrongKeys.txt") <$> getEnecuumDir
logFilePath = L.createFilePath =<< (</> "data" </> "logs") <$> getEnecuumDir
storyFilePath = L.createFilePath =<< (</> "story") <$> getEnecuumDir
appFileName = L.createFilePath =<< (</> "data" </> "logs" </> "app.log") <$> getEnecuumDir
clientStory = (</> "client.story") <$> storyFilePath
defaultLogFileName = (</> "default.log") <$> logFilePath
configFilePath = "./configs/config.json"
