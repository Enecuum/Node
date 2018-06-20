{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Service.Transaction.Storage where
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import Service.System.Directory (getLedgerFilePath, getTransactionFilePath, getMicroblockFilePath)
import Data.Default (def)
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Resource
import Control.Retry
import Control.Exception
import qualified Control.Monad.Catch as E
import qualified Control.Monad.CatchIO as I
import Control.Monad.IO.Class
import Control.Retry
import Data.Maybe
import Control.Concurrent (forkIO, threadDelay)
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import Service.Types.PublicPrivateKeyPair
import Service.Types
import Data.Pool




data DBPoolDescriptor = DBPoolDescriptor {
    poolTransaction :: Pool Rocks.DB
  , poolMicroblock :: Pool Rocks.DB
  , poolLedger :: Pool Rocks.DB }

data MacroblockDB = MacroblockDB {
  keyBlock :: BC.ByteString,
  microblockNumber :: Int,
  requiredNumberOfMicroblocks :: Int,
  hashOfMicroblock :: BC.ByteString
                                 }

-- for rocksdb Transaction and Microblock
rHash key = SHA1.hash . BC.pack . show $ key
rValue value = BC.pack $ show value
urValue value = BC.unpack value

-- for Balance Table and Ledger
htK key = BC.pack $ show key
unHtK key = read (BC.unpack key) :: PublicKey
unHtA key = read (BC.unpack key) :: Amount



-- data DBdescriptor = DBdescriptor {
--     descrDBTransaction :: Rocks.DB
--   , descrDBMicroblock :: Rocks.DB
--   , descrDBLedger :: Rocks.DB }


-- startDB :: IO DBdescriptor
-- startDB = do
--     aMicroblockPath <- getMicroblockFilePath
--     aTransactionPath <- getTransactionFilePath
--     aLedgerPath <- getLedgerFilePath
--     dbMb <- Rocks.open aMicroblockPath def{Rocks.createIfMissing=True}
--     dbTx <- Rocks.open aTransactionPath def{Rocks.createIfMissing=True}
--     dbLedger <- Rocks.open aLedgerPath def{Rocks.createIfMissing=True}
--     -- putStrLn "StartDB"
--     -- sleepMs 5000
--     -- throw DBTransactionException
--     return (DBdescriptor dbTx dbMb dbLedger)

connectRocks :: IO DBPoolDescriptor
connectRocks = do
  aTx <- getTransactionFilePath
  aMb <- getMicroblockFilePath
  aLd <- getLedgerFilePath
  poolTransaction <- createPool (Rocks.open aTx def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolMicroblock  <- createPool (Rocks.open aMb def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolLedger      <- createPool (Rocks.open aLd def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  return (DBPoolDescriptor poolTransaction poolMicroblock poolLedger)
--  fun pool


getAllValues :: MonadResource m => Rocks.DB -> m [BC.ByteString]
getAllValues db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it



data SuperException = DBTransactionException
                  | NotImplementedException -- test
                  | OtherException
                  deriving (Show)

instance Exception SuperException

--run = retry startDB

--retry :: (MonadIO m, E.MonadMask m) => RetryPolicyM m -> m a -> m a
--retry :: RetryPolicyM IO -> IO a -> IO a
--retry :: IO DBdescriptor
retry = recovering def handler . const $ connectRocks  --( startDB)   --`I.finally` closeDesc)
hmm = retrying def (const $ return . isNothing) f
f _ = putStrLn "Running action" >> return Nothing



--SomeException
handler :: [p -> E.Handler IO Bool]
handler =
    [ \_ -> E.Handler $ \(_ :: SuperException) -> do
        return True
    , \_ -> E.Handler $ \(e :: SuperException) -> do
        putStrLn ("GOT ERROR: " ++ show e)
        return True
    ]



-- handlers :: Monad m => [a -> Handler m Bool]
-- handlers =
--     [ const . Handler $ \(e :: RedisError) -> case e of
--         RedisError msg -> pure $ "READONLY" `isPrefixOf` msg
--         _              -> pure False
--     , const . Handler $ \(_ :: ConnectionError) -> pure True
--     , const . Handler $ \(_ ::         Timeout) -> pure True
--     , const . Handler $ \e ->
--         case e of
--             TransactionAborted -> pure True
--             _                  -> pure False
--     ]


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
