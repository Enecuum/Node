{-# LANGUAGE AllowAmbiguousTypes #-}

module Enecuum.Core.Database.Language where

import           Enecuum.Prelude
import           Data.Typeable (typeOf)

import qualified Enecuum.Core.Types.Database as D

-- | Interface to Key-Value database.
data DatabaseF db a where
    -- | Check whether the key exists.
    HasKeyRaw   :: D.DBKeyRaw -> (Bool -> next) -> DatabaseF db next
    -- | Lookup a value from the DB.
    GetValueRaw :: D.DBKeyRaw -> (D.DBResult D.DBValueRaw -> next) -> DatabaseF db next
    -- | Write a single value to the DB.
    PutValueRaw :: D.DBKeyRaw -> D.DBValueRaw -> (D.DBResult () -> next) -> DatabaseF db next
    -- TODO: Iterate :: ??
  deriving (Functor)

-- | Database language.
type DatabaseL db = Free (DatabaseF db)

hasKeyRaw :: D.DBKeyRaw -> DatabaseL db Bool
hasKeyRaw key = liftF $ HasKeyRaw key id

getValueRaw :: D.DBKeyRaw -> DatabaseL db (D.DBResult D.DBValueRaw)
getValueRaw key = liftF $ GetValueRaw key id

putValueRaw :: D.DBKeyRaw -> D.DBValueRaw -> DatabaseL db (D.DBResult ())
putValueRaw key val = liftF $ PutValueRaw key val id

putEntity
    :: forall entity db
    .  D.RawDBEntity db entity
    => D.DBKey entity
    -> D.DBValue entity
    -> DatabaseL db (D.DBResult ())
putEntity dbKey dbVal = let
    rawK = D.toRawDBKey   @db dbKey
    rawV = D.toRawDBValue @db dbVal
    in putValueRaw rawK rawV

putEntity'
    :: forall entity db src
    .  D.RawDBEntity db entity
    => D.ToDBKey   entity src
    => D.ToDBValue entity src
    => src
    -> DatabaseL db (D.DBResult ())
putEntity' src = let
    rawK = D.toRawDBKey   @db @entity $ D.toDBKey   src
    rawV = D.toRawDBValue @db @entity $ D.toDBValue src
    in putValueRaw rawK rawV

getEntity
    :: forall entity db
    . (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.DBKey entity
    -> DatabaseL db (D.DBResult (D.DBE entity))
getEntity dbKey = do
    let rawK = D.toRawDBKey @db dbKey
    let proxyVal = undefined :: D.DBValue entity
    eRawVal <- getValueRaw rawK
    case eRawVal of
        Left err       -> pure $ Left err
        Right rawVal   -> case D.fromRawDBValue @db rawVal of
            Nothing    -> pure $ Left $ D.DBError D.InvalidType $ show $ typeOf proxyVal
            Just dbVal -> pure $ Right (dbKey, dbVal)

getValue
    :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.DBKey entity
    -> DatabaseL db (D.DBResult (D.DBValue entity))
getValue dbKey = do
    eEntity <- getEntity dbKey
    pure $ eEntity >>= Right . snd

getValue'
    :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.ToDBKey entity src
    => src
    -> DatabaseL db (D.DBResult (D.DBValue entity))
getValue' src = do
    eEntity <- getEntity $ D.toDBKey src
    pure $ eEntity >>= Right . snd

findValue
    :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.DBKey entity
    -> DatabaseL db (Maybe (D.DBValue entity))
findValue key = do
    eVal <- getValue key
    pure $ either (const Nothing) Just eVal

findValue'
    :: (FromJSON (D.DBValue entity), D.RawDBEntity db entity, Typeable (D.DBValue entity))
    => D.ToDBKey entity src
    => src
    -> DatabaseL db (D.DBResult (Maybe (D.DBValue entity)))
findValue' src = do
    eVal <- getValue' src
    case eVal of
        Left (D.DBError D.KeyNotFound _) -> pure $ Right Nothing
        Left err                         -> pure $ Left err
        Right val                        -> pure $ Right $ Just val
