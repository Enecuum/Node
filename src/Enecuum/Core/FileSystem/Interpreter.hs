module Enecuum.Core.FileSystem.Interpreter where

import qualified Data.ByteString.Lazy  as B
import qualified Enecuum.Core.Language as L
import           Enecuum.Prelude
import           System.Directory      (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath.Posix (splitFileName)
-- import           System.FilePath  ((</>))

-- | Interpret CryptoL language.
interpretFileSystemL :: L.FileSystemF a -> IO a
interpretFileSystemL (L.ReadFile filename next) = do
    text <- B.readFile filename
    pure $ next $ text
interpretFileSystemL (L.WriteFile filename text next) = do
    B.writeFile filename text
    pure $ next ()
interpretFileSystemL (L.GetHomeDirectory next) = do
    filename <- getHomeDirectory
    pure $ next filename
interpretFileSystemL (L.CreateFilePath filepath next) = do
    let (dir, filename) = splitFileName filepath
    createDirectoryIfMissing True dir
    pure $ next filepath
interpretFileSystemL (L.DoesFileExist filepath next) = do
    isFileExist <- doesFileExist filepath
    pure $ next isFileExist

runFileSystemL :: L.FileSystemL a -> IO a
runFileSystemL = foldFree interpretFileSystemL
