{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Decode where
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import qualified Data.Serialize                        as S (Serialize, decode,
                                                             encode)
import           Service.InfoMsg                       (InfoMsg (..))
-- import           Service.Transaction.Storage
import qualified Data.ByteString.Internal              as BSI
import           Data.Default                          (def)
import           Data.Pool
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Service.Types
import           Service.Types.PublicPrivateKeyPair

funW ::  Pool Rocks.DB -> [(DBKey, DBValue)] -> IO ()
funW db aMapKeyValue = do
  let fun = (\aDb -> Rocks.write aDb def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) aMapKeyValue))
  withResource db fun


funR ::  Pool Rocks.DB -> DBKey -> IO (Maybe BSI.ByteString)
funR db key = do
  let fun = (\aDb -> Rocks.get aDb Rocks.defaultReadOptions key)
  withResource db fun


funD ::  Pool Rocks.DB -> DBKey -> IO ()
funD db key = do
  let fun = (\aDb -> Rocks.delete aDb def{Rocks.sync = True} key)
  withResource db fun


getByHash :: Pool Rocks.DB -> Hash -> IO (Maybe DBValue)
getByHash pool aHash = (\(Hash key) -> funR pool key) aHash



---- Decode
-- MacroblockBD
getKeyBlockByHash :: DBPoolDescriptor -> InChan InfoMsg -> Hash  -> IO (Maybe MacroblockBD)
getKeyBlockByHash db _ kHash = do
  mb <- getByHash (poolMacroblock db) kHash
  case mb of Nothing -> return Nothing
             Just j -> case (S.decode j :: Either String MacroblockBD) of
               Left e  -> throw (DecodeException (show e))
               Right r -> return $ Just r


--Microblock
getMicroBlockByHashDB :: DBPoolDescriptor -> Hash -> IO MicroblockBD
getMicroBlockByHashDB db mHash = do
  mbByte <- getByHash (poolMicroblock db) mHash
  case mbByte of Nothing -> throw (NoSuchMicroBlockForHash $ show mHash)
                 Just m -> case (S.decode m :: Either String MicroblockBD) of
                   Left e  -> throw (DecodeException (show e))
                   Right r -> return r

--Transaction
getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe TransactionInfo) --Transaction
getTransactionByHashDB db tHash = do
  tx <- getByHash (poolTransaction db) tHash
  case tx of Nothing -> return Nothing
             Just j -> case (S.decode j :: Either String  TransactionInfo) of
               Left e   -> throw (DecodeException (show e))
               Right rt -> return $ Just rt

-- Chain
getChain :: Common -> Number -> IO Chain
getChain (Common descr _ ) aNumber = do
  maybeV <- funR (poolSprout descr) (S.encode aNumber)
  case maybeV of
    Nothing    -> return (Nothing, Nothing)
    Just m -> case S.decode m :: Either String Chain of
      Left e  -> throw (DecodeException (show e))
      Right r -> return r

--Ledger
getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO (Maybe Amount)
getBalanceForKey db key = do
    val  <- funR (poolLedger db) (S.encode key)
    case val of Nothing -> return Nothing --putStrLn "There is no such key"
                Just v  -> case (S.decode v :: Either String Amount ) of
                    Left e  -> throw (DecodeException (show e))
                    Right b -> return $ Just b
