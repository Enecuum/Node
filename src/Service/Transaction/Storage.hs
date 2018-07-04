{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.Storage where

import           Control.Exception
import           Control.Monad                       (replicateM)
import qualified Control.Monad.Catch                 as E
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State           (StateT, evalStateT, get,
                                                      put)
import           Control.Retry
import qualified Crypto.Hash.SHA256                  as SHA
import           Data.Aeson
import qualified Data.ByteString.Base64              as Base64
import qualified Data.ByteString.Char8               as BC
import qualified Data.ByteString.Internal            as BSI
import           Data.Default                        (def)
import           Data.Maybe
import           Data.Pool
import qualified Data.Serialize                      as S (Serialize, decode, encode)
import qualified "rocksdb-haskell" Database.RocksDB  as Rocks
import           Service.System.Directory            (getLedgerFilePath,
                                                      getMacroblockFilePath,
                                                      getMicroblockFilePath,
                                                      getTransactionFilePath)
import           Service.Transaction.TransactionsDAG (genNNTx)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON         ()

--------------------------------------
-- begin of the Connection section
data DBPoolDescriptor = DBPoolDescriptor {
    poolTransaction :: Pool Rocks.DB
  , poolMicroblock  :: Pool Rocks.DB
  , poolLedger      :: Pool Rocks.DB
  , poolMacroblock  :: Pool Rocks.DB
  }

-- FIX change def (5 times)
connectOrRecoveryConnect :: IO DBPoolDescriptor
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

-- data Macroblock = Macroblock {
--   keyBlock :: BC.ByteString,
--   -- microblockNumber :: Int,
--   -- requiredNumberOfMicroblocks :: Int,
--   hashOfMicroblock :: [BC.ByteString]
--   -- timeOfMicroblockArrived :: UTCTime
--                                  } deriving (Generic, Eq, Ord, Show, S.Serialize)



-- for rocksdb Transaction and Microblock
rHash :: S.Serialize a => a -> BSI.ByteString
rHash key = Base64.encode . SHA.hash . rValue $ key

rValue :: S.Serialize a => a -> BSI.ByteString
rValue value = S.encode value

urValue :: S.Serialize a => BSI.ByteString -> Either String a
urValue value = S.decode value

-- for Balance Table and Ledger
htK :: S.Serialize a => a -> BSI.ByteString
htK key = S.encode key


-- unHtK key = read (S.decode key) :: PublicKey
-- unHtA key = read (S.decode key) :: Amount


-- end of the Database structure  section
--------------------------------------


getTxsMicroblock :: DBPoolDescriptor -> MicroblockBD -> IO [Transaction]
getTxsMicroblock db (MicroblockBD _ _ _ txHashes _) = do
  let fun kTransactionHash = (\local_db -> Rocks.get local_db Rocks.defaultReadOptions kTransactionHash)
  maybeTxUntiped  <- mapM (\k -> withResource (poolTransaction db) (fun k)) txHashes
  let txDoesNotExist = filter (\t -> t /= Nothing) maybeTxUntiped
  if null txDoesNotExist
    then error "Some of transactions can not be found"
    else do
         let txUntiped = map fromJust (filter (isJust) maybeTxUntiped)
             extract t = case S.decode t :: Either String TransactionInfo of
                            Left _  -> error "Can not decode TransactionInfo"
                            Right r -> r
             txDecoded = map extract txUntiped
             tx = map (\t -> _tx  (t :: TransactionInfo)) txDecoded
         return tx


getNTransactions ::  IO [BSI.ByteString]
getNTransactions = runResourceT $ do
  let pathT = "./try.here" --"/tmp/haskell-rocksDB6"
  (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
  getNFirstValues db 100

test01 :: IO ()
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

getNFirstValuesT :: StateT Rocks.Iterator IO BSI.ByteString
getNFirstValuesT = do
  it <- get
  Just v <- lift $ Rocks.iterValue it
  lift $ Rocks.iterNext it
  put it
  return v

getNFirstValues :: (Control.Monad.Trans.Class.MonadTrans t, MonadResource (t IO)) =>
                           Rocks.DB -> Int -> t IO [BSI.ByteString]
getNFirstValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  vs <- lift $ evalStateT (replicateM n getNFirstValuesT) it
  return vs


getNLastValuesT :: StateT Rocks.Iterator IO BSI.ByteString
getNLastValuesT = do
  it <- get
  Just v <- lift $ Rocks.iterValue it
  -- v <- lift $ Rocks.iterValue it
  -- case v of Left _ -> return Nothing
  --           Right r -> do
  --             lift $ Rocks.iterPrev it
  --             put it
  --             return v
  lift $ Rocks.iterPrev it
  put it
  return v
getNLastValues :: (Control.Monad.Trans.Class.MonadTrans t, MonadResource (t IO)) =>
                          Rocks.DB -> Int -> t IO [BSI.ByteString]
getNLastValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  vs <- lift $ evalStateT (replicateM n getNLastValuesT) it
  return vs

test02 :: IO ()
test02 = do
  let path = "/tmp/haskell-rocksDB5"
  db <- Rocks.open path def{Rocks.createIfMissing=True}
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "-" "zero"]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "a" "one"
                                        , Rocks.Put "b" "two"
                                        , Rocks.Put "c" "three"
                                        , Rocks.Put "d" "four"]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "e" "five"]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "f" "six"]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "g" "seven"]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "h" "eight"]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put "j" "nine"]
  result <- Rocks.get db  Rocks.defaultReadOptions "a"
  Rocks.close db
  putStrLn $ show result



-- test03 ::  IO [BSI.ByteString]
test03 :: MonadUnliftIO m => (Rocks.DB -> t -> ResourceT m a) -> t -> m a
test03 fun n  = runResourceT $ do
  let pathT = "/tmp/haskell-rocksDB5"
  (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
  fun db n

goFirst :: Int -> Int -> IO [BSI.ByteString]
goFirst count offset = drop offset <$> test03 getNFirstValues (offset + count )
goLast :: Int -> Int -> IO [BSI.ByteString]
goLast count offset = drop offset <$> test03 getNLastValues  (offset + count )

goGetAll :: IO ()
goGetAll = do
  result <- getAll "/tmp/haskell-rocksDB5"
  print result


-- goF = goFirst 4 1
-- goL = goLast 4 1


--------------------------------------
-- begin of the Test section
-- end of the Test section
--------------------------------------

getMicroBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe MicroblockBD)
getMicroBlockByHashDB db mHash = do
  mbByte <- getByHash (poolMicroblock db) mHash
  let mb = case mbByte of Nothing -> Nothing
                          Just m -> case (S.decode m :: Either String MicroblockBD) of
                            Left _   -> error "Can not decode Microblock"
                            Right rm -> Just rm

  return mb


getBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe MicroblockAPI)
getBlockByHashDB db hash = do
  mb <- getMicroBlockByHashDB db hash
  case mb of
    Nothing -> return Nothing
    Just m@(MicroblockBD {..}) -> do
      tx <- getTxsMicroblock db m
      let txAPI = map (\t -> TransactionAPI {_tx = t, _txHash = rHash t}) tx
      let mbAPI = MicroblockAPI {
            _prevMicroblock = "",
            _nextMicroblock = "",
            _keyBlock,
            _signAPI = _signBD,
            _teamKeys,
            _publisher = read "1" :: PublicKey,
            _transactionsAPI = txAPI
            }
      return (Just mbAPI)


getKeyBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe MacroblockAPI)
getKeyBlockByHashDB db kHash = do
  kb <- getByHash (poolMacroblock db) kHash
  let t = case kb of Nothing -> Nothing
                     Just j -> case (S.decode j :: Either String Macroblock) of
                       Left _  -> error "Can not decode Macroblock"
                       Right (Macroblock {..}) -> Just a
                         where a = MacroblockAPI {
                                 _prevKBlock,
                                 _nextKBlock = "",
                                 _difficulty,
                                 _height,
                                 _solver,
                                 _reward,
                                 _txsCnt = 0,
                                 _mblocks
                                 }

  return t


getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe TransactionInfo) --Transaction
getTransactionByHashDB db tHash = do
  tx <- getByHash (poolTransaction db) tHash
  let t = case tx of Nothing -> Nothing
                     Just j -> case (S.decode j :: Either String  TransactionInfo) of
                       Left _   -> error "Can not decode TransactionInfo"
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

getAllValues :: MonadUnliftIO m => Rocks.DB -> m [BSI.ByteString]
getAllValues db = runResourceT $ getAllValues1 db

getAllValues1 :: MonadResource m => Rocks.DB -> m [BSI.ByteString]
getAllValues1 db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it

getAllItems :: MonadResource m => Rocks.DB -> m [(BSI.ByteString, BSI.ByteString)]
getAllItems db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterItems it

getAllTransactionsDB :: DBPoolDescriptor -> PublicKey -> IO [TransactionAPI]
getAllTransactionsDB descr pubKey = do
  txByte <- withResource (poolTransaction descr) getAllValues
  -- putStrLn $ show txByte
  let fun = \t -> case (S.decode t :: Either String TransactionInfo) of
                       Left _   -> error "Can not decode TransactionInfo"
                       Right rt -> Just rt

      txInfo = map fun txByte
      txWithouMaybe = map fromJust (filter (isJust) txInfo)
      tx = map (\t -> _tx (t  :: TransactionInfo) ) txWithouMaybe
      txWithKey = filter (\t -> (_owner t == pubKey || _receiver t == pubKey)) tx
      txAPI = map (\t -> TransactionAPI { _tx = t, _txHash = rHash t}) txWithKey
  return txAPI

getAllTransactions :: IO ()
getAllTransactions = do
  result <- getAll =<< getTransactionFilePath
  let func res = case (S.decode res :: Either String TransactionInfo) of
        Right r -> r
        Left _  -> error "Can not decode Transaction"
  let result2 = map func result
  putStrLn $ show result2
  -- return result2


getAll ::  String -> IO [BSI.ByteString]
getAll path = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  getAllValues db


getAllKV ::  String -> IO [(BSI.ByteString,BSI.ByteString)]
getAllKV path = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  getAllItems db


getAllLedger :: IO ()
getAllLedger = do
  result <- getAll =<< getLedgerFilePath
  -- let result2 = map (\res -> S.decode res :: Either String PublicKey) result
  let func res = case (S.decode res :: Either String Amount) of
        Right r -> r
        Left _  -> error "Can not decode Ledger"
  let result2 = map func result
  putStrLn $ show result2




getAllMicroblocks :: IO ()
getAllMicroblocks = do
  result <- getAll =<< getMicroblockFilePath
  let func res = case (S.decode res :: Either String Microblock) of
        Right r -> r
        Left _  -> error "Can not decode Microblock"
  let result2 = map func result
  putStrLn $ show result2


getAllLedgerKV :: IO ()
getAllLedgerKV = do
  result <- getAllKV =<< getLedgerFilePath
  -- let result2 = map (\res -> S.decode res :: Either String PublicKey) result
  let func res = case (S.decode res :: Either String Amount) of
        Right r -> r
        Left _  -> error "Can not decode Ledger"
  let result2 = map (\(k,v) -> (k, func v)) result
  putStrLn $ show result2


getAllTransactionsKV :: IO ()
getAllTransactionsKV = do
  result <- getAllKV =<< getTransactionFilePath
  let func res = case (S.decode res :: Either String Transaction) of
        Right r -> r
        Left _  -> error "Can not decode Transaction"
  let result2 = map (\(k,v) -> (k, func v)) result
  putStrLn $ show result2


getAllMicroblockKV :: IO [(BSI.ByteString, MicroblockBD)]
getAllMicroblockKV = do
  result <- getAllKV =<< getMicroblockFilePath
  let func res = case (S.decode res :: Either String MicroblockBD) of
        Right r -> r
        Left _  -> error "Can not decode Microblock"
  let result2 = map (\(k,v) -> (k, func v)) result
  -- putStrLn $ show result2
  return result2


getAllMacroblockKV :: IO ()
getAllMacroblockKV = do
  result <- getAllKV =<< getMacroblockFilePath
  let func res = case (S.decode res :: Either String Macroblock) of
        Right r -> r
        Left _  -> error "Can not decode Macroblock"
  let result2 = map (\(k,v) -> (k, func v)) result
  putStrLn $ show result2



-- end of the Query Iterator section
--------------------------------------

-- showAllMicroblockKV :: IO [(Text, MicroblockBD)]
-- showAllMicroblockKV = do
--           mbs <- getAllMicroblockKV
--           return $ map (\(bs, mb) -> (decodeUtf8 $ encodeBase58 bitcoinAlphabet bs, mb)) mbs

--------------------------------------
-- begin test cli

getOneMicroblock :: IO ()
getOneMicroblock = do
  c <- connectDB
  let h = Hash ("LGseiNy5K6dk989PB7ICNQ0XqAoZwc776EO74x5oucE=" :: BSI.ByteString)
  -- let h = Hash ("\248\198\199\178e\ETXt\186T\148y\223\224t-\168p\162\138\&1" :: BSI.ByteString)
  mb <- getBlockByHashDB c h
  print mb


getOneTransaction :: IO ()
getOneTransaction = do
  c <- connectDB
  let h = Hash ("JafY+7bQi3E1/9EMGe2hrrYqGRKWI8isi5giKkEkf0c=" :: BSI.ByteString)
  tx <- getTransactionByHashDB c h
  print tx


getTransactionsByKey :: IO ()
getTransactionsByKey = do
  c <- connectDB
  tx <- getAllTransactionsDB c (read "QYy3AT4a3Z88MpEoGDixRgxtWW8v3RfSbJLFQEyFZwMe" :: PublicKey)
  print tx

-- end test cli
--------------------------------------


getOneKeyBlock :: IO (Maybe MacroblockAPI)
getOneKeyBlock = do
  c <- connectDB
  let h = Hash ("XXX" :: BSI.ByteString)
  getKeyBlockByHashDB c h

tryParseTXInfoJson :: IO ()
tryParseTXInfoJson = do
  tx <- genNNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2
  let eti = Data.Aeson.encode ti
  print eti
  let res = Data.Aeson.decode eti :: Maybe TransactionInfo
  -- return t
  print $ res


tryParseTXInfoBin :: IO ()
tryParseTXInfoBin = do
  tx <- genNNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2
  let eti = S.encode ti
  print eti
  let res = S.decode eti :: Either String TransactionInfo
  -- return t
  print $ res
