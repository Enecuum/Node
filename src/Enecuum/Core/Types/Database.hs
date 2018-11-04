{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Core.Types.Database where

import           Enecuum.Prelude
import           Data.Aeson.Extra     (noLensPrefix)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

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

class DBModelEntity db entity => RawDBEntity db entity where
    toRawDBKey     :: DBKey   entity -> DBKeyRaw
    toRawDBValue   :: DBValue entity -> DBValueRaw
    fromRawDBValue :: DBValueRaw -> Maybe (DBValue entity)

    -- TODO: this doesn't work by some strange reason.
    default toRawDBValue :: ToJSON (DBValue entity) => DBValue entity -> DBValueRaw
    toRawDBValue = LBS.toStrict . A.encode
    default fromRawDBValue :: FromJSON (DBValue entity) => DBValueRaw -> Maybe (DBValue entity)
    fromRawDBValue = A.decode . LBS.fromStrict

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

toDBEntity
    :: (ToDBKey entity src, ToDBValue entity src)
    => src
    -> DBE entity
toDBEntity src = (toDBKey src, toDBValue src)