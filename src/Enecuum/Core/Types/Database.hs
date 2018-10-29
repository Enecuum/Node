{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude

type DBValueRaw = ByteString
type DBKeyRaw   = ByteString

type DBIndex = Int

data DBKey   db spec = DBKey   DBKeyRaw
data DBValue db spec = DBValue DBValueRaw

data DBErrorType
    = DBSystemError
    | KeyNotFound
    | NotFound
    | InvalidType
    deriving (Generic, Ord, Eq, Show, Read)

data DBError = DBError DBErrorType Text
    deriving (Generic, Ord, Eq, Show, Read)

type DBResult a = Either DBError a

data Storage db = Storage
    { _path :: FilePath
    }

data DBOptions = DBOptions
    { _createIfMissing :: Bool
    , _errorIfExists   :: Bool
    }

data DBConfig db = DBConfig
    { _path    :: FilePath
    , _options :: DBOptions
    }

defaultDbOptions :: DBOptions
defaultDbOptions = DBOptions
    { _createIfMissing = False
    , _errorIfExists   = False
    }
