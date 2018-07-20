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

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Internal              as BSI
import           Data.Default                          (def)
import qualified Data.Serialize                        as S (decode, encode)
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Service.System.Directory              (getLedgerFilePath,
                                                        getMacroblockFilePath,
                                                        getMicroblockFilePath,
                                                        getTransactionFilePath)
import           Service.Transaction.TransactionsDAG   (genNNTx)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()

getOneMicroblock :: IO ()
getOneMicroblock = undefined
-- getOneMicroblock = do
--   c <- connectDB
--   let h = Hash ("2c4kS21m0cA91siB0OfuoCwLRWKyLl/2yuk3z+6BvFY=" :: BSI.ByteString)
--   -- let h = Hash ("\248\198\199\178e\ETXt\186T\148y\223\224t-\168p\162\138\&1" :: BSI.ByteString)
--   mb <- getBlockByHashDB c h
--   print mb


getOneTransaction :: IO ()
getOneTransaction = do
  c <- connectDB
  let h = Hash ("NKgaAanEuIfiXBWWPzQVSCjkWu1Y/xVLS4o7IIAVt1E=" :: BSI.ByteString)
  tx <- getTransactionByHashDB c h
  print tx


getTransactionsByKey :: IO ()
getTransactionsByKey = do
  c <- connectDB
  tx <- getAllTransactionsDB c (read "QYy3AT4a3Z88MpEoGDixRgxtWW8v3RfSbJLFQEyFZwMe" :: PublicKey)
  print tx


-- mb <- B.getKeyBlockByHashDB db hash aInfoChan

-- end test cli
--------------------------------------

getOneKeyBlock2 :: IO (Maybe MacroblockAPI)
getOneKeyBlock2 = do
  c <- connectDB
  let h = Hash ("MzI=" :: BSI.ByteString)
  -- let h = Hash ("32" :: BSI.ByteString)
  (inChan, _) <- newChan 64
  getKeyBlockByHashDB c h inChan
-- "MzI="

getOneKeyBlock :: IO (Maybe MacroblockBD)
getOneKeyBlock = do
  c <- connectDB
  -- let h = Hash ("32" :: BSI.ByteString)
  mb <- funR (poolMacroblock c) "32"
  case mb of Nothing -> do
               -- writeLog aInfoChan [BDTag] Error ("No Team Keys For Key block " ++ show aHash)
               return Nothing
             Just j -> case (S.decode j :: Either String MacroblockBD) of
               Left _  -> error "Can not decode Macroblock"
               Right r -> return $ Just r

-- getOneKeyBlock = do
--   c <- connectDB
--   let h = Hash ("XXX" :: BSI.ByteString)
--   getKeyBlockByHashDB c h

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


--------------------------------------

-- showAllMicroblockKV :: IO [(Text, MicroblockBD)]
-- showAllMicroblockKV = do
--           mbs <- getAllMicroblockKV
--           return $ map (\(bs, mb) -> (decodeUtf8 $ encodeBase58 bitcoinAlphabet bs, mb)) mbs

--------------------------------------
-- begin test cli


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


-- data TypeInfo = Amount | Transaction | MicroblockBD | MacroblockBD --

-- getAllKV1 :: TypeInfo -> IO ()
-- getAllKV1 Amount       = getLedgerFilePath
-- getAllKV1 Transaction{}  = getTransactionFilePath
-- getAllKV1 MicroblockBD{} = getMicroblockFilePath
-- getAllKV1 MacroblockBD{} = getMacroblockFilePath

-- getAllKV2 typeInfo = do
--   result <- getAllKV =<< getAllKV1 typeInfo


getAllLedgerKV :: IO ()
getAllLedgerKV = do
  result <- getAllKV =<< getLedgerFilePath
  let func res = case (S.decode res :: Either String Amount) of
        Right r -> r
        Left _  -> error "Can not decode Ledger"
  let result2 = map (\(k,v) -> (k, func v)) result
  print result2


getAllTransactionsKV :: IO ()
getAllTransactionsKV = do
  result <- getAllKV =<< getTransactionFilePath
  let func res = case (S.decode res :: Either String Transaction) of
        Right r -> r
        Left _  -> error "Can not decode Transaction"
  let result2 = map (\(k,v) -> (k, func v)) result
  print result2


getAllMicroblockKV :: IO ()
getAllMicroblockKV = do
  result <- getAllKV =<< getMicroblockFilePath
  let func res = case (S.decode res :: Either String MicroblockBD) of
        Right r -> r
        Left _  -> error "Can not decode Microblock"
  let result2 = map (\(k,v) -> (k, func v)) result
  print result2
  -- return result2


getAllMacroblockKV :: IO ()
getAllMacroblockKV = do
  result <- getAllKV =<< getMacroblockFilePath
  let func res = case (S.decode res :: Either String MacroblockBD) of
        Right r -> r
        Left _  -> error "Can not decode MacroblockBD"
  let result2 = map (\(k,v) -> (k, func v)) result
  print result2



-- end of the Query Iterator section



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
