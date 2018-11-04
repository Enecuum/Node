module Enecuum.Assets.System.Directory where

import qualified Enecuum.Core.Language as L
import           Enecuum.Prelude
import           System.FilePath       ((</>))

getEnecuumDir :: (L.FileSystem m, Monad m) => m FilePath
getEnecuumDir = L.createFilePath =<< (</> ".enecuum") <$> L.getHomeDirectory

keysFilePath :: (L.FileSystem m, Monad m) => m FilePath
keysFilePath = (</> "keys.txt") <$> getEnecuumDir

wrongKeysFilePath :: (L.FileSystem m, Monad m) => m FilePath
wrongKeysFilePath = (</> "wrongKeys.txt") <$> getEnecuumDir

logFilePath :: (L.FileSystem m, Monad m) => m FilePath
logFilePath = L.createFilePath =<< (</> "data" </> "logs") <$> getEnecuumDir

storyFilePath :: (L.FileSystem m, Monad m) => m FilePath
storyFilePath = L.createFilePath =<< (</> "story") <$> getEnecuumDir

appFileName :: (L.FileSystem m, Monad m) => m FilePath
appFileName = L.createFilePath =<< (</> "data" </> "logs" </> "app.log") <$> getEnecuumDir

clientStory :: (L.FileSystem m, Monad m) => m FilePath
clientStory = (</> "client.story") <$> storyFilePath
