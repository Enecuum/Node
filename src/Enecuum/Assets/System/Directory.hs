module Enecuum.Assets.System.Directory where

import           Enecuum.Prelude
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath  ((</>))


getEnecuumDir :: IO FilePath
getEnecuumDir = createFilePath =<< liftM (</> ".enecuum") getHomeDirectory

createFilePath :: FilePath -> IO FilePath
createFilePath file = do
    createDirectoryIfMissing True file
    pure file

logFilePath :: IO FilePath
logFilePath = createFilePath =<< liftM (</> "data" </> "logs") getEnecuumDir

storyFilePath :: IO FilePath
storyFilePath = createFilePath =<< liftM (</> "story") getEnecuumDir

appFileName :: IO FilePath
appFileName = liftM (</> "app.log") logFilePath

clientStory :: IO FilePath
clientStory = liftM (</> "client.story") storyFilePath

defaultLogFileName :: IO FilePath
defaultLogFileName = liftM (</> "default.log") logFilePath

configFilePath :: FilePath
configFilePath = "./configs/config.json"
