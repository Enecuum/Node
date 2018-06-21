{-# LANGUAGE PackageImports, ScopedTypeVariables, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}
module Service.Transaction.Storage where
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import Service.System.Directory (getLedgerFilePath, getTransactionFilePath, getMicroblockFilePath)
import Data.Default (def)
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Resource
import Control.Retry
import Control.Exception
import qualified Control.Monad.Catch as E
import Control.Monad.IO.Class
import Control.Retry
import Data.Maybe
import Control.Concurrent (forkIO, threadDelay)
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import Service.Types.PublicPrivateKeyPair
import Service.Types
import Data.Pool
import Data.Time.Clock (getCurrentTime, UTCTime)
-- import qualified Database.Persist.Postgresql as Post
-- import qualified Database.PostgreSQL.Simple as Post
import qualified Data.ByteString.Internal as BSI
import Control.Monad.Trans.State (StateT, evalStateT, put, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad (replicateM)
import Data.Aeson
import GHC.Generics
import Data.Serialize (Serialize)

data DBPoolDescriptor = DBPoolDescriptor {
    poolTransaction :: Pool Rocks.DB
  , poolMicroblock :: Pool Rocks.DB
  , poolLedger :: Pool Rocks.DB
  , poolMacroblock :: Pool Rocks.DB
  }

data Macroblock = Macroblock {
  keyBlock :: BC.ByteString,
  -- microblockNumber :: Int,
  -- requiredNumberOfMicroblocks :: Int,
  hashOfMicroblock :: [BC.ByteString]
  -- timeOfMicroblockArrived :: UTCTime
                                 } deriving (Generic, Eq, Ord, Show, Serialize)



-- for rocksdb Transaction and Microblock
rHash key = SHA1.hash . BC.pack . show $ key
rValue value = BC.pack $ show value
urValue value = BC.unpack value

-- for Balance Table and Ledger
htK key = BC.pack $ show key
unHtK key = read (BC.unpack key) :: PublicKey
unHtA key = read (BC.unpack key) :: Amount



connectDB :: IO DBPoolDescriptor
connectDB = do
  aTx <- getTransactionFilePath
  aMb <- getMicroblockFilePath
  aLd <- getLedgerFilePath
  let aMacroblock = ""
  poolTransaction <- createPool (Rocks.open aTx def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolMicroblock  <- createPool (Rocks.open aMb def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolLedger      <- createPool (Rocks.open aLd def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolMacroblock  <- createPool (Rocks.open aMacroblock def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  -- putStrLn "DBTransactionException"
  -- sleepMs 5000
  -- throw DBTransactionException
  return (DBPoolDescriptor poolTransaction poolMicroblock poolLedger poolMacroblock)






getNTransactions ::  IO [BSI.ByteString]
getNTransactions = runResourceT $ do
  let pathT = "./try.here" --"/tmp/haskell-rocksDB6"
  (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
  getNValues db 100

test01 = do
  let path = "/tmp/haskell-rocksDB6"
  db <- Rocks.open path def{Rocks.createIfMissing=True}
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put (BC.pack "a") (BC.pack "one")
                                        , Rocks.Put (BC.pack "b") (BC.pack "two")
                                        , Rocks.Put (BC.pack "c") (BC.pack "three") ]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put (BC.pack "a") (BC.pack "4")]
  result <- Rocks.get db Rocks.defaultReadOptions (BC.pack "a")
  Rocks.close db
  putStrLn $ show result


--genNMicroBlocksV1 :: Int -> IO [MicroblockV1]
-- getNValuesTN n = evalStateT (replicateM n getNValuesT) BC.empty


getNValuesT :: StateT Rocks.Iterator IO BSI.ByteString
getNValuesT = do
  it <- get
  Just v <- lift $ Rocks.iterValue it
  lift $ Rocks.iterNext it
  put it
  return v

getNValues :: MonadResource m => Rocks.DB -> p -> m [BSI.ByteString]
getNValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Just v1 <- Rocks.iterValue it

  -- vs <- evalStateT (replicateM n getNValuesT) it
  -- return vs

  Rocks.iterNext it
  Just v2 <- Rocks.iterValue it
  return [v1,v2]


getAllValues db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it











data SuperException = DBTransactionException
                    | NotImplementedException -- test
                    | OtherException
                  deriving (Show)

instance Exception SuperException


-- FIX change def (5 times)
connectOrRecoveryConnect = recovering def handler . const $ connectDB


--catch all exceptions and retry connections
handler :: [p -> E.Handler IO Bool]
handler =
    [ \_ -> E.Handler $ \(_ :: SomeException) -> do
        return True
    ]



-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)


getBlockByHashDB :: DBPoolDescriptor -> Hash -> IO Microblock
getBlockByHashDB db mHash = do
  let (Hash key) = mHash
  let fun = \db -> Rocks.get db Rocks.defaultReadOptions key
  (Just v)  <- withResource (poolMicroblock db) fun
  return (read (urValue v) :: Microblock)


getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO TransactionInfo --Transaction
getTransactionByHashDB db tHash = do
  let (Hash key) = tHash
  let fun = \db -> Rocks.get db Rocks.defaultReadOptions key
  (Just v)  <- withResource (poolTransaction db) fun
  return (read (urValue v) :: TransactionInfo)


deleteMicroblocksByHash :: DBPoolDescriptor -> [BC.ByteString] -> IO ()
deleteMicroblocksByHash db hashes = deleteByHash (poolMicroblock db) hashes


deleteTransactionsByHash :: DBPoolDescriptor -> [BC.ByteString] -> IO ()
deleteTransactionsByHash db hashes = deleteByHash (poolTransaction db) hashes


deleteByHash :: Pool Rocks.DB -> [BC.ByteString] -> IO ()
deleteByHash pool tHash = do
  let fun k = (\db -> Rocks.delete db def{Rocks.sync = True} k)
  mapM_ (\k ->  withResource pool (fun k)) tHash
