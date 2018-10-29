module Enecuum.Core.Database.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language as L


-- | Interpret CryptoL language.
interpretDatabaseL :: L.FileSystemF a -> IO a
interpretDatabaseL (L.ReadFile filename next) = do
    text <- readFile filename
    pure $ next text
interpretDatabaseL (L.GetHomeDirectory next) = do
    filename <- getHomeDirectory 
    pure $ next filename 
interpretDatabaseL (L.CreateFilePath file next) = do
    createDirectoryIfMissing True file
    pure $ next file

runDatabaseL :: L.FileSystemL a -> IO a
runDatabaseL = foldFree interpretDatabaseL

    