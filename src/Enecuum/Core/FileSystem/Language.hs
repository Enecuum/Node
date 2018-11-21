{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.FileSystem.Language where

import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as BSI
-- import qualified Data.ByteString.Internal  as BSI

-- | Language for FileSystem.
data FileSystemF next where
    ReadFile :: FilePath -> (BSI.ByteString -> next) -> FileSystemF next
    WriteFile :: FilePath -> BSI.ByteString -> (() -> next) -> FileSystemF next
    GetHomeDirectory :: (FilePath -> next) -> FileSystemF next
    CreateFilePath :: FilePath -> (FilePath -> next) -> FileSystemF next
    DoesFileExist :: FilePath -> (Bool -> next) -> FileSystemF next
makeFunctorInstance ''FileSystemF

type FileSystemL next = Free FileSystemF next

class FileSystem m where
    readFile :: FilePath -> m B.ByteString
    writeFile :: FilePath -> B.ByteString -> m ()
    getHomeDirectory :: m FilePath
    createFilePath :: FilePath -> m FilePath
    doesFileExist :: FilePath -> m Bool

instance FileSystem (Free FileSystemF) where
    readFile filename = liftF $ ReadFile filename id
    writeFile filename text = liftF $ WriteFile filename text id
    getHomeDirectory = liftF $ GetHomeDirectory id
    createFilePath filepath = liftF $ CreateFilePath filepath id
    doesFileExist filepath = liftF $ DoesFileExist filepath id
