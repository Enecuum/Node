{-# LANGUAGE PackageImports #-}

module Service.Transaction.Storage where
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import Service.System.Directory (getLedgerFilePath, getTransactionFilePath, getMicroblockFilePath)
import Data.Default (def)
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Resource
data DBdescriptor = DBdescriptor {
    descrDBTransaction :: Rocks.DB
  , descrDBMicroblock :: Rocks.DB
  , descrDBLedger :: Rocks.DB }



startDB :: IO DBdescriptor
startDB = do
    aMicroblockPath <- getMicroblockFilePath
    aTransactionPath <- getTransactionFilePath
    aLedgerPath <- getLedgerFilePath
    dbMb <- Rocks.open aMicroblockPath def{Rocks.createIfMissing=True}
    dbTx <- Rocks.open aTransactionPath def{Rocks.createIfMissing=True}
    dbLedger <- Rocks.open aLedgerPath def{Rocks.createIfMissing=True}
    return (DBdescriptor dbTx dbMb dbLedger)


getAllValues :: MonadResource m => Rocks.DB -> m [BC.ByteString]
getAllValues db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it
