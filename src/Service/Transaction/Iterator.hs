{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}

module Service.Transaction.Iterator where
import           Control.Monad                       (replicateM, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State           (StateT, evalStateT, get,
                                                      put)
-- import           Data.Maybe
-- import qualified Control.Concurrent                  as C
import qualified Data.ByteString.Char8               as BC
import           Data.Default                        (def)
import qualified Data.Serialize                      as S (encode)
import qualified "rocksdb-haskell" Database.RocksDB  as Rocks
import           Service.Transaction.Decode
import           Service.Transaction.TransactionsDAG
import           Service.Types
import           Service.Types.PublicPrivateKeyPair

getNFirstValuesT :: StateT Rocks.Iterator IO DBValue
getNFirstValuesT = do
  it <- get
  Just v <- Rocks.iterValue it
  Rocks.iterNext it
  put it
  return v


getNFirstValues :: (MonadTrans t, MonadResource (t IO)) => Rocks.DB -> Int -> t IO [DBValue]
getNFirstValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  lift $ evalStateT (replicateM n getNFirstValuesT) it


getNLastValuesT :: StateT Rocks.Iterator IO DBValue
getNLastValuesT = do
  it <- get
  Just v <- Rocks.iterValue it
  Rocks.iterPrev it
  put it
  return v


getNLastValues :: Rocks.DB -> Int -> IO [DBValue]
getNLastValues db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  lift $ evalStateT (replicateM n getNLastValuesT) it


getFirst :: (MonadResource (t IO), MonadTrans t) => Rocks.DB -> Int -> Int -> t IO [DBValue]
getFirst db offset count = drop offset <$> getNFirstValues db (offset + count )

getLast :: Rocks.DB -> Int -> Int -> IO [(DBKey, DBValue)]
getLast db  offset count = drop offset <$> getNLastValues2 db (offset + count )


getNLastValues2 :: Rocks.DB -> Int -> IO [(DBKey, DBValue)]
getNLastValues2 db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  _ <- replicateM (n - 1) $ Rocks.iterPrev it
  Rocks.iterItems it


findTransactionsForWallet1 :: Rocks.DB -> PublicKey -> Int -> Int -> IO [TransactionAPI]
findTransactionsForWallet1 db pubKey offset aCount = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  lift $ findTransactionsForWallet it pubKey aCount 10


findLastValue :: Rocks.Iterator -> IO (Maybe DBValue)
findLastValue it = do
  Rocks.iterLast it
  v <- Rocks.iterValue it
  Rocks.iterPrev it
  return v


test07 :: IO ()
test07 = do
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
  Rocks.close db


g7 = runResourceT $ do
  let path = "/tmp/haskell-rocksDB7"
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  it <- Rocks.iterOpen db Rocks.defaultReadOptions
  lift (findLastValue it)
  -- Rocks.close db


findUntil :: Rocks.Iterator -> Int -> Int -> (DBValue -> Bool) -> IO [DBValue]
findUntil it count maximum' predicate = do
-- if (count > 0 && maximum' > 0)
  rawTx <- findLastValue it
  case rawTx of
    Nothing -> return []
    Just j  -> if (predicate j)
      then findUntil it count (maximum' - 1) predicate
      else findUntil it (count - 1) (maximum' - 1) predicate


findTransactionsForWallet :: Rocks.Iterator -> PublicKey -> Int -> Int -> IO [TransactionAPI]
findTransactionsForWallet it pubKey count maximum' = do
  txInfo <- findUntil it count maximum' (decodeAndFilter pubKey)
  let tx = map (\t -> _tx (decodeThis "TransactionInfo" t :: TransactionInfo)) txInfo
  return $ map (\t -> TransactionAPI { _tx = t, _txHash = rHashT t}) tx


testTxForWallet :: IO ()
testTxForWallet = do
  -- create tx
  tx <- genNTx 6
  -- print tx
  -- print $ length tx
  let pubKey = read "KX8GHxYaejRMEmDrM7ZrbMudvKY57rwW1u4PZN9s3z1z" :: PublicKey
      txKV = zip [1..] tx
      funTx (k,t)
        | even k = t {_owner = pubKey }
        | odd k = t
      mTx = map funTx txKV
  -- print txKV
  -- print mTx
  print $ map (txFilterByKey pubKey) mTx
  let ti = \t -> TransactionInfo t (BC.pack "123") 2 False
      kv = map (\t -> ((rHashT t),(S.encode $ ti t))) mTx
      kvB = map (\(k,v) -> Rocks.Put k v ) kv
      path = "/tmp/txInfo18"

  db <- Rocks.open path def{Rocks.createIfMissing=True}
  -- write tx to db
  Rocks.write db def{Rocks.sync = True} kvB

  -- read tx from db
  -- result <- mapM (Rocks.get db  Rocks.defaultReadOptions) (map fst kv)
  -- print $ map (\r -> (decodeRaw r) :: Maybe TransactionInfo) result

  -- find values in db
  what <- findTransactionsForWallet1 db pubKey 0 3
  print $ "we have found: " ++ (show $ length what) ++ "Transactions"
  Rocks.close db
