{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude
import           Data.Aeson.Extra (noLensPrefix)

type DBKeyRaw   = ByteString
type DBValueRaw = ByteString

class DB db where
    getDbName :: FilePath

class DBEntity entity where
    data DBKey   entity :: *
    data DBValue entity :: *

class DBEntity entity => ToDBKey entity src where
    toDBKey :: src -> DBKey entity

class DBEntity entity => ToDBValue entity src where
    toDBValue :: src -> DBValue entity

class (DB db, DBEntity entity) => DBModelEntity db entity

class DBModelEntity db entity => GetRawDBEntity db entity where
    getRawDBKey   :: DBKey   entity -> DBKeyRaw
    getRawDBValue :: DBValue entity -> DBValueRaw

type DBE entity = (DBKey entity, DBValue entity)

data DBErrorType
    = SystemError
    | KeyNotFound
    | InvalidType
    deriving (Generic, Ord, Eq, Enum, Bounded, Show, Read)

data DBError = DBError DBErrorType Text
    deriving (Generic, Ord, Eq, Show, Read)

type DBResult a = Either DBError a

data Storage db = Storage
    { _path :: FilePath
    }
    deriving (Show, Generic)

instance ToJSON   (Storage db) where toJSON    = genericToJSON    noLensPrefix
instance FromJSON (Storage db) where parseJSON = genericParseJSON noLensPrefix

data DBOptions = DBOptions
    { _createIfMissing :: Bool
    , _errorIfExists   :: Bool
    }
    deriving (Show, Generic)

instance ToJSON   DBOptions where toJSON    = genericToJSON    noLensPrefix
instance FromJSON DBOptions where parseJSON = genericParseJSON noLensPrefix

data DBConfig db = DBConfig
    { _path    :: FilePath
    , _options :: DBOptions
    }
    deriving (Show, Generic)

instance ToJSON   (DBConfig db) where toJSON    = genericToJSON    noLensPrefix
instance FromJSON (DBConfig db) where parseJSON = genericParseJSON noLensPrefix

defaultDbOptions :: DBOptions
defaultDbOptions = DBOptions
    { _createIfMissing = False
    , _errorIfExists   = False
    }
