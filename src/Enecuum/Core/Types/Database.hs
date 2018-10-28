{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude

type DBValueRaw = LByteString
type DBKeyRaw   = LByteString

type DBIndex = Int

data DBKey   db spec = DBKey   DBKeyRaw
data DBValue db spec = DBValue DBValueRaw

data DBErrorType
    = KeyNotFound
    | NotFound
    | InvalidType
    deriving (Generic, Ord, Eq, Show, Read)

data DBError = DBError DBErrorType Text

data Storage db = Storage FilePath

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
