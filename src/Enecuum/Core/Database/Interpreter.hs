{-# LANGUAGE PackageImports #-}

module Enecuum.Core.Database.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Core.Types as D
import qualified "rocksdb-haskell" Database.RocksDB as Rocks


-- TODO: think about read / write options.
-- https://task.enecuum.com/issues/2859

-- | Interpret DatabaseL language.
interpretDatabaseL :: Rocks.DB -> L.DatabaseF db a -> IO a

-- TODO: Perhaps, this method can be implemented more effectively with using Bloom filter.
-- For now, it's just the same as
interpretDatabaseL db (L.HasKey key next) = do
    mbVal <- Rocks.get db Rocks.defaultReadOptions key
    pure $ next $ isJust mbVal

interpretDatabaseL db (L.GetValue key next) = do
    mbVal <- Rocks.get db Rocks.defaultReadOptions key
    pure $ next $ case mbVal of
        Nothing  -> Left $ D.DBError D.KeyNotFound (show key)
        Just val -> Right val

interpretDatabaseL db (L.PutValue key val next) = do
    -- TODO: catch exceptions, if any
    Rocks.put db Rocks.defaultWriteOptions key val
    pure $ next $ Right ()

runDatabaseL ::  Rocks.DB -> L.DatabaseL db a -> IO a
runDatabaseL db = foldFree (interpretDatabaseL db)
