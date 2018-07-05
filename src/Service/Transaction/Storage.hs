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
import           Control.Monad                      (replicateM)
import qualified Control.Monad.Catch                as E
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State          (StateT, evalStateT, get,
                                                     put)
import           Control.Retry
import qualified Crypto.Hash.SHA256                 as SHA
import qualified Data.ByteString.Base64             as Base64
-- import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Internal           as BSI
import           Data.Default                       (def)
import           Data.Maybe
import           Data.Pool
import qualified Data.Serialize                     as S (Serialize, decode,
                                                          encode)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Service.System.Directory           (getLedgerFilePath,
                                                     getMacroblockFilePath,
                                                     getMicroblockFilePath,
                                                     getTransactionFilePath)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON        ()

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




-- End of the Connection section
--------------------------------------


--------------------------------------
-- begin of the Database structure  section

-- for rocksdb Transaction and Microblock
rHash :: S.Serialize a => a -> BSI.ByteString
rHash key = Base64.encode . SHA.hash . S.encode $ key


funW db aMapKeyValue = do
  let fun = (\aDb -> Rocks.write aDb def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) aMapKeyValue))
  withResource db fun


funR db key = do
  let fun = (\aDb -> Rocks.get aDb Rocks.defaultReadOptions key)
  withResource db fun
-- end of the Database structure  section
--------------------------------------



getTxs :: DBPoolDescriptor -> MicroblockBD -> IO [TransactionInfo]
getTxs desc (MicroblockBD _ _ _ txHashes _) = do
  maybeTxUntiped  <- mapM (funR (poolTransaction desc)) txHashes
  let txDoesNotExist = filter (\t -> t /= Nothing) maybeTxUntiped
  if null txDoesNotExist
    then error "Some of transactions can not be found"
    else do
         let txUntiped = map fromJust (filter (isJust) maybeTxUntiped)
             extract t = case S.decode t :: Either String TransactionInfo of
                            Left _  -> error "Can not decode TransactionInfo"
                            Right r -> r
             txDecoded = map extract txUntiped
             -- tx = map (\t -> _tx  (t :: TransactionInfo)) txDecoded
         return txDecoded


getTxsMicroblock :: DBPoolDescriptor -> MicroblockBD -> IO [Transaction]
getTxsMicroblock db mb@(MicroblockBD _ _ _ _ _) = do
  txDecoded <- getTxs db mb
  let tx = map (\t -> _tx  (t :: TransactionInfo)) txDecoded
  return tx


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
  lift $ Rocks.iterPrev it
  put it
  return v

getNLastValues :: (MonadTrans t, MonadResource (t IO)) =>
                          Rocks.DB -> Int -> t IO [BSI.ByteString]
getNLastValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  vs <- lift $ evalStateT (replicateM n getNLastValuesT) it
  return vs

getFirst :: (MonadResource (t IO), MonadTrans t) =>
                    Rocks.DB -> Int -> Int -> t IO [BSI.ByteString]
getFirst db offset count = drop offset <$> getNFirstValues db (offset + count )
getLast db  offset count = drop offset <$> getNLastValues db (offset + count )



getLastTransactions :: DBPoolDescriptor -> PublicKey -> Int -> Int -> IO [TransactionAPI]
getLastTransactions _ pubKey _ _ = do
  -- let fun = \db -> getLast db offset count
  -- rawTxInfo <- withResource (poolTransaction descr) fun
  let rawTxInfo = undefined
  let txAPI = decodeTransactionsAndFilterByKey rawTxInfo pubKey
  return txAPI


getMicroBlockByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe MicroblockBD)
getMicroBlockByHashDB db mHash = do
  mbByte <- getByHash (poolMicroblock db) mHash
  let mb = case mbByte of Nothing -> Nothing
                          Just m -> case (S.decode m :: Either String MicroblockBD) of
                            Left _   -> error "Can not decode Microblock"
                            Right rm -> Just rm

  return mb


getTransactionsByMicroblockHash :: DBPoolDescriptor -> Hash -> IO (Maybe [TransactionInfo])
getTransactionsByMicroblockHash db aHash = do
  mb <- getMicroBlockByHashDB db aHash
  case mb of
    Nothing -> return Nothing
    Just m@(MicroblockBD {..}) -> do
      txInfo <- getTxs db m
      return $ Just txInfo


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


getByHash :: Pool Rocks.DB -> Hash -> IO (Maybe BSI.ByteString)
getByHash pool hash = do
  let (Hash key) = hash
  funR pool key


decodeTransactionsAndFilterByKey :: [BSI.ByteString] -> PublicKey -> [TransactionAPI]
decodeTransactionsAndFilterByKey rawTx pubKey = txAPI
  where fun = \t -> case (S.decode t :: Either String TransactionInfo) of
                       Left _   -> error "Can not decode TransactionInfo"
                       Right rt -> Just rt

        txInfo = map fun rawTx
        txWithouMaybe = map fromJust (filter (isJust) txInfo)
        tx = map (\t -> _tx (t  :: TransactionInfo) ) txWithouMaybe
        txWithKey = filter (\t -> (_owner t == pubKey || _receiver t == pubKey)) tx
        txAPI = map (\t -> TransactionAPI { _tx = t, _txHash = rHash t}) txWithKey


getAllTransactionsDB :: DBPoolDescriptor -> PublicKey -> IO [TransactionAPI]
getAllTransactionsDB descr pubKey = do
  txByte <- withResource (poolTransaction descr) getAllValues
  let txAPI = decodeTransactionsAndFilterByKey txByte pubKey
  return txAPI


getAllValues :: MonadUnliftIO m => Rocks.DB -> m [BSI.ByteString]
getAllValues db = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it


getAllItems :: MonadResource m => Rocks.DB -> m [(BSI.ByteString, BSI.ByteString)]
getAllItems db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterItems it
