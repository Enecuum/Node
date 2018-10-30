module Enecuum.Core.Database.Language where

import           Enecuum.Prelude

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

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

putValue :: D.GetRawDBEntity entity => D.DBKey entity -> D.DBValue entity -> DatabaseL db (D.DBResult ())
putValue dbKey dbVal = let
    (rawK, rawV) = getRawDBEntity dbKey dbVal
    in putValueRaw rawK rawV

getEntity
    :: (FromJSON (D.DBValue entity), D.GetRawDBEntity entity, Typeable (D.DBValue entity))
    => D.DBKey entity
    -> DatabaseL db (D.DBResult (D.DBE entity))
getEntity dbKey = do
    eRawVal <- getValueRaw $ D.getRawDBKey dbKey
    pure $ eRawVal >>= parseDBValue >>= (\dbVal -> Right (dbKey, dbVal))

getValue
    :: (FromJSON (D.DBValue entity), D.GetRawDBEntity entity, Typeable (D.DBValue entity))
    => D.DBKey entity
    -> DatabaseL db (D.DBResult (D.DBValue entity))
getValue dbKey = do
    eEntity <- getEntity dbKey
    pure $ eEntity >>= Right . snd

findValue
    :: (FromJSON (D.DBValue entity), D.GetRawDBEntity entity, Typeable (D.DBValue entity))
    => D.DBKey entity
    -> DatabaseL db (Maybe (D.DBValue entity))
findValue key = do
    eVal <- getValue key
    pure $ either (const Nothing) Just eVal

getRawDBEntity :: D.GetRawDBEntity entity => D.DBKey entity -> D.DBValue entity -> (D.DBKeyRaw, D.DBValueRaw)
getRawDBEntity dbKey dbVal = (D.getRawDBKey dbKey, D.getRawDBValue dbVal)

-- TODO: type of a
parseDBValue :: (Typeable a, FromJSON a) => D.DBValueRaw -> D.DBResult a
parseDBValue raw = case A.decode $ LBS.fromStrict raw of
    Nothing  -> Left $ D.DBError D.InvalidType ""           -- TODO: type of a
    Just val -> Right val


