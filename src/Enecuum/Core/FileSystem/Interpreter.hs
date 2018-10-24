module Enecuum.Core.FileSystem.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language     as L
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath  ((</>))

-- | Interpret CryptoL language.
interpretFileSystemL :: L.FileSystemF a -> IO a
interpretFileSystemL (L.ReadFile filename next) = do
    text <- readFile filename
    pure $ next text
interpretFileSystemL (L.GetHomeDirectory next) = do
    filename <- getHomeDirectory 
    pure $ next filename 
interpretFileSystemL (L.CreateFilePath file next) = do
    createDirectoryIfMissing True file
    pure $ next file

runFileSystemL :: L.FileSystemL a -> IO a
runFileSystemL = foldFree interpretFileSystemL

    