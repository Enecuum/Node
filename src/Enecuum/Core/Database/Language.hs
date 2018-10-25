module Enecuum.Core.Database.Language where

import           Enecuum.Prelude

import qualified Data.Aeson as A
import           Data.Typeable (typeOf)
import           Data.Proxy (Proxy)

import qualified Data.HGraph.StringHashable as D
import qualified Enecuum.Core.Types.Database as D

-- | Interface to Key-Value database.
data DatabaseF db a where
    Contains :: D.DBKeyRaw -> (Bool -> next) -> DatabaseF db next
    -- | Write a single value to the DB.
    PutValue :: D.DBKeyRaw -> D.DBValueRaw -> (() -> next) -> DatabaseF db next
    -- | Lookup a value from the DB.
    GetValue :: D.DBKeyRaw -> (Maybe D.DBValueRaw -> next) -> DatabaseF db next
    -- Iterate :: ??
  deriving (Functor)

-- | Database language.
type DatabaseL db = Free (DatabaseF db)

class Monad m => Database m where
    contains :: D.DBKeyRaw -> m Bool
    putValue :: D.DBKeyRaw -> D.DBValueRaw -> m ()
    getValue :: D.DBKeyRaw -> m (Maybe D.DBValueRaw)

instance Database (DatabaseL db) where
    contains key     = liftF $ Contains key id
    putValue key val = liftF $ PutValue key val id
    getValue key     = liftF $ GetValue key id

-- findValue :: forall a m. (Typeable a, FromJSON a, Database m) => D.DBKey -> m (Either D.DBError a)
-- findValue key = do
--     mbRaw <- getValue key
--     case mbRaw of
--         Nothing  -> pure $ Left $ D.KeyNotFound key
--         Just raw -> case A.decode raw of
--             Nothing  -> pure $ Left $ D.InvalidType $ show $ typeOf (Proxy :: Proxy a)
--             Just val -> pure $ Right val

-- class ToDBKey a where
--     toDBKey :: a -> D.DBKey

-- instance ToDBKey D.StringHash where
--     toDBKey = show . D.fromStringHash

-- hasKey :: (ToDBKey a, Database m) => a -> m Bool
-- hasKey = contains . toDBKey