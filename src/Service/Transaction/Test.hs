{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.Test where

import           Service.Transaction.Storage

-- import           Control.Concurrent.Chan.Unagi.Bounded
-- import           Control.Exception                   (throw)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Char8               as BC
import qualified Data.ByteString.Internal            as BSI
import           Data.Default                        (def)
import qualified Data.Serialize                      as S (decode, encode)
import qualified Data.Serialize                      as S
import qualified "rocksdb-haskell" Database.RocksDB  as Rocks
import           Service.System.Directory            (getLedgerFilePath,
                                                      getMacroblockFilePath,
                                                      getMicroblockFilePath,
                                                      getSproutFilePath,
                                                      getTransactionFilePath)
import           Service.Transaction.Decode
import           Service.Transaction.Iterator
import           Service.Transaction.TransactionsDAG (genNNTx)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON         ()


tryParseTXInfoJson :: IO ()
tryParseTXInfoJson = do
  tx <- genNNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2 False
  let eti = Data.Aeson.encode ti
  print eti
  let res = Data.Aeson.decode eti :: Maybe TransactionInfo
  -- return res
  print $ res


tryParseTXInfoBin :: IO TransactionInfo
tryParseTXInfoBin = do
  tx <- genNNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2 False
  let eti = S.encode ti
  print eti
  let res = decodeThis "TransactionInfo" eti
  return res


getAllTransactions :: IO [TransactionInfo]
getAllTransactions = do
  result <- getAll =<< getTransactionFilePath
  return $ map (decodeThis "TransactionInfo") result


getAll ::  String -> IO [DBValue]
getAll path = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  getAllValues db


getAllKV ::  String -> IO [(DBKey,DBValue)]
getAllKV path = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  getAllItems db


getAllLedger :: IO [Amount]
getAllLedger = do
  l <- getAllLedgerKV
  return $ map snd l


getAllMicroblocks :: IO [MicroblockBD]
getAllMicroblocks = do
  m <- getAllMicroblockKV
  return $ map snd m


getAllAndDecode :: IO String -> (DBKey -> a) -> (DBValue -> b) -> IO [(a, b)]
getAllAndDecode filename funcK funcV = do
  result <- getAllKV =<< filename
  let result2 = map (\(k,v) -> (funcK k, funcV v)) result
  return result2


getAllAndDecode2 :: (S.Serialize b, S.Serialize a) => IO String -> DecodeType -> DecodeType -> IO [(a, b)]
getAllAndDecode2 filename aType bType = getAllAndDecode filename (decodeThis aType) (decodeThis bType)


getAllSproutKV :: IO [(Integer, (Maybe MainChain, Maybe SproutChain))]
getAllSproutKV = getAllAndDecode2 getSproutFilePath "Integer" "(Maybe MainChain, Maybe SproutChain)"


getAllLedgerKV :: IO [(PublicKey,Amount)]
getAllLedgerKV = getAllAndDecode2 getLedgerFilePath "PublicKey" "Amount"


getAllTransactionsKV :: IO [(HashOfTransaction,TransactionInfo)]
getAllTransactionsKV = getAllAndDecode getTransactionFilePath id (decodeThis "TransactionInfo")


getAllMicroblockKV :: IO [(HashOfMicroblock,MicroblockBD)]
getAllMicroblockKV = getAllAndDecode getMicroblockFilePath id (decodeThis "MicroblockBD")


getAllMacroblockKV :: IO [(HashOfKeyBlock,MacroblockBD)]
getAllMacroblockKV = getAllAndDecode getMacroblockFilePath id (decodeThis "MacroblockBD")


-- test03 ::  IO [BSI.ByteString]
test03 :: MonadUnliftIO m => (Rocks.DB -> t -> ResourceT m a) -> t -> m a
test03 fun n  = runResourceT $ do
  let pathT = "/tmp/haskell-rocksDB5"
  (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
  fun db n


goGetAll :: IO ()
goGetAll = do
  result <- getAll "/tmp/haskell-rocksDB5"
  print result



--------------------------------------
-- begin of the Test section

-- goF = goFirst 4 1
-- goL = goLast 4 1

test02 :: IO ()
test02 = do
  let path = "/tmp/haskell-rocksDB7"
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
  print result

  -- let something1 = \db -> runResourceT $ do
  --       it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  --       Rocks.iterLast it
  --       return it
  -- iter <- something1 db
  -- let something2 = \it -> runResourceT $ do
  --       Rocks.iterPrev it
  --       Rocks.iterItems it
  -- value <- something2 iter

  let something3 = \bd -> runResourceT $ do
        it    <- Rocks.iterOpen bd Rocks.defaultReadOptions
        Rocks.iterLast it
        -- Rocks.iterPrev it
        Rocks.iterItems it
  value <- something3 db
  print value

  result1 <- Rocks.get db  Rocks.defaultReadOptions "a"
  print result1

  let something4 = \bd -> runResourceT $ do
        it    <- Rocks.iterOpen bd Rocks.defaultReadOptions
        Rocks.iterLast it
        -- Rocks.iterPrev it
        Rocks.iterItems it
  value4 <- something4 db

  result2 <- Rocks.get db  Rocks.defaultReadOptions "a"
  print result2

  print value4
  Rocks.close db


test01 :: IO ()
test01 = do
  let path = "/tmp/haskell-rocksDB6"
  db <- Rocks.open path def{Rocks.createIfMissing=True}
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put (BC.pack "a") (BC.pack "one")
                                        , Rocks.Put (BC.pack "b") (BC.pack "two")
                                        , Rocks.Put (BC.pack "c") (BC.pack "three") ]
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put (BC.pack "a") (BC.pack "4")]
  result1 <- Rocks.get db Rocks.defaultReadOptions (BC.pack "a")
  putStrLn $ show result1
  Rocks.delete db def{Rocks.sync = True} (BC.pack "a")
  result2 <- Rocks.get db Rocks.defaultReadOptions (BC.pack "a")
  putStrLn $ show result2
  Rocks.close db



-- test04 :: IO ()
-- test04 = runResourceT $ do
--   let pathT = "/tmp/haskell-rocksDB5"
--   (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
--   getLast db 0 10


getO ::  IO [BSI.ByteString]
getO = runResourceT $ do
  let pathT = "/tmp/haskell-rocksDB5"
  (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
  getNValuesOriginal db

getS ::  IO [(BSI.ByteString, BSI.ByteString)]
getS = runResourceT $ do
  let pathT = "/tmp/haskell-rocksDB5"
  (_, db) <- Rocks.openBracket pathT def{Rocks.createIfMissing=False}
  getNValues db 3
  -- getLast2 db 100

getNValues :: MonadResource m => Rocks.DB -> Int -> m [(BSI.ByteString, BSI.ByteString)]
getNValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  _ <- replicateM (n - 1) $ Rocks.iterPrev it
  Rocks.iterItems it



getNValuesOriginal :: MonadResource m => Rocks.DB -> m [BSI.ByteString]
getNValuesOriginal db  = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  Just v1 <- Rocks.iterValue it
  Rocks.iterPrev it
  Just v2 <- Rocks.iterValue it
  return [v1,v2]
