module Enecuum.Core.Database.Language where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types.Database as D

-- | Interface to Key-Value database.
data DatabaseF db a where
    -- | Check whether the key exists.
    HasKey   :: D.DBKeyRaw -> (Bool -> next) -> DatabaseF db next
    -- | Lookup a value from the DB.
    GetValue :: D.DBKeyRaw -> (D.DBResult D.DBValueRaw -> next) -> DatabaseF db next
    -- | Write a single value to the DB.
    PutValue :: D.DBKeyRaw -> D.DBValueRaw -> (() -> next) -> DatabaseF db next
    -- TODO: Iterate :: ??
  deriving (Functor)

-- | Database language.
type DatabaseL db = Free (DatabaseF db)

class Monad m => Database m where
    hasKey   :: D.DBKeyRaw -> m Bool
    getValue :: D.DBKeyRaw -> m (D.DBResult D.DBValueRaw)
    putValue :: D.DBKeyRaw -> D.DBValueRaw -> m ()

instance Database (DatabaseL db) where
    hasKey   key     = liftF $ HasKey   key id
    getValue key     = liftF $ GetValue key id
    putValue key val = liftF $ PutValue key val id
