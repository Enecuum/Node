{-# LANGUAGE PackageImports, ScopedTypeVariables, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}
module Service.Transaction.Storage where
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import Service.System.Directory (getLedgerFilePath, getTransactionFilePath, getMicroblockFilePath, getMacroblockFilePath)
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
import qualified Data.ByteString.Internal as BSI
import Control.Monad.Trans.State (StateT, evalStateT, put, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad (replicateM)
import Data.Aeson
import GHC.Generics
import qualified Data.Serialize as S (Serialize, encode, decode)
import Service.Transaction.TransactionsDAG (genNNTx)
import Data.Typeable

--------------------------------------
-- begin of the Connection section
data DBPoolDescriptor = DBPoolDescriptor {
    poolTransaction :: Pool Rocks.DB
  , poolMicroblock :: Pool Rocks.DB
  , poolLedger :: Pool Rocks.DB
  , poolMacroblock :: Pool Rocks.DB
  }

-- FIX change def (5 times)
connectOrRecoveryConnect = recovering def handler . const $ connectDB


connectDB :: IO DBPoolDescriptor
connectDB = do
  aTx <- getTransactionFilePath
  aMb <- getMicroblockFilePath
  aLd <- getLedgerFilePath
  aMacroblock <- getMacroblockFilePath
  poolTransaction <- createPool (Rocks.open aTx def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolMicroblock  <- createPool (Rocks.open aMb def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolLedger      <- createPool (Rocks.open aLd def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolMacroblock  <- createPool (Rocks.open aMacroblock def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  -- putStrLn "DBTransactionException"
  -- sleepMs 5000
  -- throw DBTransactionException
  return (DBPoolDescriptor poolTransaction poolMicroblock poolLedger poolMacroblock)


data SuperException = DBTransactionException
                    | NotImplementedException -- test
                    | OtherException
                  deriving (Show)

instance Exception SuperException


--catch all exceptions and retry connections
handler :: [p -> E.Handler IO Bool]
handler =
    [ \_ -> E.Handler $ \(_ :: SomeException) -> do
        return True
    ]


-- -- A utility function - threadDelay takes microseconds, which is slightly annoying.
-- sleepMs n = threadDelay (n * 1000)

-- End of the Connection section
--------------------------------------


--------------------------------------
-- begin of the Database structure  section

data Macroblock = Macroblock {
  keyBlock :: BC.ByteString,
  -- microblockNumber :: Int,
  -- requiredNumberOfMicroblocks :: Int,
  hashOfMicroblock :: [BC.ByteString]
  -- timeOfMicroblockArrived :: UTCTime
                                 } deriving (Generic, Eq, Ord, Show, S.Serialize)



-- for rocksdb Transaction and Microblock
rHash key = SHA1.hash . rValue $ key
rValue value = S.encode value
urValue value = S.decode value

-- for Balance Table and Ledger
htK key = S.encode key
unA balance = case (urValue balance) of Left _ -> error "Can not decode balance"
                                        Right b -> return $ Just (read b :: Amount)

-- unHtK key = read (S.decode key) :: PublicKey
-- unHtA key = read (S.decode key) :: Amount


-- end of the Database structure  section
--------------------------------------






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





--------------------------------------
-- begin of the Test section
-- end of the Test section
--------------------------------------

getMicroBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe MicroblockAPI)
getMicroBlockByHashDB db mHash = do
  mbByte <- getByHash (poolMicroblock db) mHash
  let mb = case mbByte of Nothing -> Nothing
                          Just m -> case (urValue m) of
                            Left _ -> error "Can not decode Microblock"
                            Right mt -> Just (read mt :: Microblock)
  -- let mb = read (urValue mbByte) :: Maybe Microblock
  let mbAPI = read (show mb) :: Maybe MicroblockAPI
  return mbAPI


getKeyBlockByHashDB = undefined


getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe TransactionInfo) --Transaction
getTransactionByHashDB db tHash = do
  tx <- getByHash (poolTransaction db) tHash
  let t = case tx of Nothing -> Nothing
                     Just t -> case (urValue t) of
                       Left _ -> error "Can not decode TransactionInfo"
                       Right rt -> Just (read rt :: TransactionInfo)
  return t


deleteMicroblocksByHash :: DBPoolDescriptor -> [BC.ByteString] -> IO ()
deleteMicroblocksByHash db hashes = deleteByHash (poolMicroblock db) hashes


deleteTransactionsByHash :: DBPoolDescriptor -> [BC.ByteString] -> IO ()
deleteTransactionsByHash db hashes = deleteByHash (poolTransaction db) hashes




getByHash :: Pool Rocks.DB -> Hash -> IO (Maybe BSI.ByteString)
getByHash pool hash = do
  let (Hash key) = hash
  let fun = \db -> Rocks.get db Rocks.defaultReadOptions key
  withResource pool fun


deleteByHash :: Pool Rocks.DB -> [BC.ByteString] -> IO ()
deleteByHash pool hash = do
  let fun k = (\db -> Rocks.delete db def{Rocks.sync = True} k)
  mapM_ (\k ->  withResource pool (fun k)) hash


--------------------------------------
-- begin of the Query section

-- get all values from the table via iterator
-- getAllValues :: MonadResource m => Rocks.DB -> m [BSI.ByteString]
getAllValues db = runResourceT $ getAllValues1 db
getAllValues1 db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it



-- getAllTransactionsDB :: DBPoolDescriptor -> PublicKey -> IO [Transaction]
getAllTransactionsDB = undefined
-- getAllTransactionsDB descr pubKey = do
--   txByte <- withResource (poolTransaction descr) getAllValues
--   -- let txInfo = case txByte of [] -> []
--   --                             tInfo -> read (urValue tInfo) :: [TransactionInfo]
--   tx <- genNNTx 5
--   return tx
--   -- return txByte



-- end of the Query section
--------------------------------------
