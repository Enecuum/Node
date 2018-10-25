module Enecuum.Core.Database.Language where

import           Enecuum.Prelude

import qualified Data.Aeson as A
import           Data.Typeable (typeOf)
import           Data.Proxy (Proxy)

import qualified Data.HGraph.StringHashable as D
import qualified Enecuum.Core.Database.Types as D

-- | Interface to Key-Value database.
data DatabaseF t a where
    Contains :: D.DBKey -> (Bool -> next) -> DatabaseF t next
    -- | Write a single value to the DB.
    PutValue :: D.DBKey -> D.DBValue -> (() -> next) -> DatabaseF t next
    -- | Lookup a value from the DB.
    GetValue :: D.DBKey -> (Maybe D.DBValue -> next) -> DatabaseF t next
    -- Iterate :: ??
  deriving (Functor)

-- | Database language.
type DatabaseL t = Free (DatabaseF t)

class Monad m => Database m where
    contains   :: D.DBKey -> m Bool
    putValue :: D.DBKey -> D.DBValue -> m ()
    getValue :: D.DBKey -> m (Maybe D.DBValue)

instance Database (DatabaseL t) where
    contains key     = liftF $ Contains key id
    putValue key val = liftF $ PutValue key val id
    getValue key     = liftF $ GetValue key id

findValue :: forall a m. (Typeable a, FromJSON a, Database m) => D.DBKey -> m (Either D.DBError a)
findValue key = do
    mbRaw <- getValue key
    case mbRaw of
        Nothing  -> pure $ Left $ D.KeyNotFound key
        Just raw -> case A.decode raw of
            Nothing  -> pure $ Left $ D.InvalidType $ show $ typeOf (Proxy :: Proxy a)
            Just val -> pure $ Right val

class ToDBKey a where
    toDBKey :: a -> D.DBKey

instance ToDBKey D.StringHash where
    toDBKey = show . D.fromStringHash

hasKey :: (ToDBKey a, Database m) => a -> m Bool
hasKey = contains . toDBKey