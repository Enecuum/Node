{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.FileSystem.Language where

import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor

-- | Language for FileSystem.
data FileSystemF next where
    ReadFile :: FilePath -> (Text -> next) -> FileSystemF next
    GetHomeDirectory :: (FilePath -> next) -> FileSystemF next
    CreateFilePath :: FilePath -> (FilePath -> next) -> FileSystemF next
makeFunctorInstance ''FileSystemF
  
type FileSystemL next = Free FileSystemF next
  
class FileSystem m where
    readFile :: FilePath -> m Text
    getHomeDirectory :: m FilePath
    createFilePath :: FilePath -> m FilePath 

instance FileSystem (Free FileSystemF) where
    readFile filename = liftF $ ReadFile filename id    
    getHomeDirectory = liftF $ GetHomeDirectory id
    createFilePath filepath = liftF $ CreateFilePath filepath id