{-# LANGUAGE PackageImports, ScopedTypeVariables, FlexibleContexts, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
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
unA balance = case (urValue balance :: Either String Amount ) of
  Left _ -> error ("Can not decode balance" ++ show balance)
  Right b -> return $ Just b

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

getMicroBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe Microblock)
getMicroBlockByHashDB db mHash = do
  mbByte <- getByHash (poolMicroblock db) mHash
  let mb = case mbByte of Nothing -> Nothing
                          Just m -> case (S.decode m :: Either String Microblock) of
                            Left _ -> error "Can not decode Microblock"
                            Right rm -> Just rm

  return mb

getBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe MicroblockAPI)
getBlockByHashDB db hash = do
  mb <- getMicroBlockByHashDB db hash
  let mbAPI = read (show mb) :: Maybe MicroblockAPI
  return mbAPI

getKeyBlockByHashDB = undefined


getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe TransactionInfo) --Transaction
getTransactionByHashDB db tHash = do
  tx <- getByHash (poolTransaction db) tHash
  let t = case tx of Nothing -> Nothing
                     Just j -> case (S.decode j :: Either String  TransactionInfo) of
                       Left _ -> error "Can not decode TransactionInfo"
                       Right rt -> Just rt
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
-- begin of the Query Iterator section

-- get all values from the table via iterator
-- getAllValues :: MonadResource m => Rocks.DB -> m [BSI.ByteString]
getAllValues db = runResourceT $ getAllValues1 db
getAllValues1 db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it

getAllItems db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterItems it


getAllTransactionsDB descr pubKey = do
  txByte <- withResource (poolTransaction descr) getAllValues
  let fun = \t -> case (S.decode t :: Either String TransactionInfo) of
                       Left _ -> error "Can not decode TransactionInfo"
                       Right rt -> Just rt

  let txInfo = map fun txByte
  let txWithouMaybe = map fromJust (filter (isJust) txInfo)
  let tx = map _tx txWithouMaybe
  let txWithKey = filter (\t -> (_owner t == pubKey || _receiver t == pubKey)) tx
  return txWithKey

getAllTransactions = do
  result <- getAll =<< getTransactionFilePath
  let func res = case (S.decode res :: Either String Transaction) of
        Right r -> r
        Left _ -> error "Can not decode Transaction"
  let result2 = map func result
  -- putStrLn $ show result2
  return result2


getAll ::  String -> IO [BSI.ByteString]
getAll path = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  getAllValues db


getAllKV ::  String -> IO [(BSI.ByteString,BSI.ByteString)]
getAllKV path = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  getAllItems db



getAllLedger = do
  result <- getAll =<< getLedgerFilePath
  -- let result2 = map (\res -> S.decode res :: Either String PublicKey) result
  let func res = case (S.decode res :: Either String Amount) of
        Right r -> r
        Left _ -> error "Can not decode Ledger"
  let result2 = map func result
  putStrLn $ show result2





getAllMicroblocks = do
  result <- getAll =<< getMicroblockFilePath
  let func res = case (S.decode res :: Either String Microblock) of
        Right r -> r
        Left _ -> error "Can not decode Microblock"
  let result2 = map func result
  putStrLn $ show result2


getAllLedgerKV = do
  result <- getAllKV =<< getLedgerFilePath
  -- let result2 = map (\res -> S.decode res :: Either String PublicKey) result
  let func res = case (S.decode res :: Either String Amount) of
        Right r -> r
        Left _ -> error "Can not decode Ledger"
  let result2 = map (\(k,v) -> (k, func v)) result
  putStrLn $ show result2


getAllTransactionsKV = do
  result <- getAllKV =<< getTransactionFilePath
  let func res = case (S.decode res :: Either String Transaction) of
        Right r -> r
        Left _ -> error "Can not decode Transaction"
  let result2 = map (\(k,v) -> (k, func v)) result
  putStrLn $ show result2


getAllMicroblockKV = do
  result <- getAllKV =<< getMicroblockFilePath
  let func res = case (S.decode res :: Either String Microblock) of
        Right r -> r
        Left _ -> error "Can not decode Microblock"
  let result2 = map (\(k,v) -> (k, func v)) result
  -- putStrLn $ show result2
  return result2
-- end of the Query Iterator section
--------------------------------------


--------------------------------------
-- begin test cli

getOneMicroblock = do
  c <- connectDB
  let h = Hash ("\247\206\247\163v\n\176g\222Jl\202\DC1s\179\189aY\145h" :: BSI.ByteString)
  -- let h = Hash ("\248\198\199\178e\ETXt\186T\148y\223\224t-\168p\162\138\&1" :: BSI.ByteString)
  mb <- getMicroBlockByHashDB c h
  print mb


getOneTransaction = do
  c <- connectDB
  let h = Hash ("\244\US%\FS`\243\202\192\171\136m\235\237\199\224A\171\212C\149" :: BSI.ByteString)
  tx <- getTransactionByHashDB c h
  print tx


getTransactionsByKey = do
  c <- connectDB
  tx <- getAllTransactionsDB c (read "QYy3AT4a3Z88MpEoGDixRgxtWW8v3RfSbJLFQEyFZwMe" :: PublicKey)
  print tx

-- end test cli
--------------------------------------
