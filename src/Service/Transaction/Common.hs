{-# LANGUAGE PackageImports #-}
module Service.Transaction.Common where
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import Service.Transaction.Storage (DBdescriptor(..))
import Service.Types
import Service.Transaction.Storage (urValue)

getBlockByHashDB :: DBdescriptor -> Hash -> IO Microblock
getBlockByHashDB db mHash = do
  let (Hash key) = mHash
  (Just v)  <- Rocks.get (descrDBMicroblock db) Rocks.defaultReadOptions key
  return (read (urValue v) :: Microblock)


getTransactionByHashDB :: DBdescriptor -> Hash -> IO TransactionInfo --Transaction
getTransactionByHashDB db tHash = do
  let (Hash key) = tHash
  (Just v)  <- Rocks.get (descrDBTransaction db) Rocks.defaultReadOptions key
  return (read (urValue v) :: TransactionInfo)
