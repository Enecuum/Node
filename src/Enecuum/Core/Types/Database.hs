{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude

type DBValueRaw = ByteString
type DBKeyRaw   = ByteString

class DBEntity entity src where
    data DBKey   entity :: *
    data DBValue entity :: *
    toDBKey   :: src -> DBKey   entity
    toDBValue :: src -> DBValue entity

class GetRawDBEntity entity where
    getRawDBKey   :: DBKey   entity -> DBKeyRaw
    getRawDBValue :: DBValue entity -> DBValueRaw

type DBE spec = (DBKey spec, DBValue spec)

data DBErrorType
    = DBSystemError
    | KeyNotFound
    | NotFound
    | InvalidType
    deriving (Generic, Ord, Eq, Enum, Bounded, Show, Read)

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
