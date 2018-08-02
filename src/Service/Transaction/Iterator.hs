{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Iterator where
import           Control.Monad                       (replicateM, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State           (StateT, evalStateT, get,
                                                      put)
-- import           Data.Maybe
-- import qualified Control.Concurrent                  as C
import           Control.Exception                   (throw)
import qualified Data.ByteString.Char8               as BC
import           Data.Default                        (def)
import qualified Data.Map                            as M
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


getNLastValuesT :: StateT Rocks.Iterator IO DBValue
getNLastValuesT = do
  it <- get
  value <- Rocks.iterValue it
  Rocks.iterPrev it
  put it
  case value of
    Nothing -> throw NoValueInDBAnymore
    Just j  -> return j


findLastValue :: Rocks.Iterator -> IO (Maybe DBValue)
findLastValue it = do
  Rocks.iterLast it
  v <- Rocks.iterValue it
  Rocks.iterPrev it
  return v


findUntil2 :: StateT (Rocks.Iterator, Int, Int, DBValue -> Bool) IO (Maybe DBValue)
findUntil2  = do
  (it, count, maximum', predicate) <- get
  if count > 0 && maximum' > 0
    then do
    -- print $ "count " ++ show count ++ " maximum' " ++ show maximum'
    Rocks.iterLast it
    rawTx <- Rocks.iterValue it
    Rocks.iterPrev it
    case rawTx of
      Nothing -> return Nothing
      Just j  -> if predicate j
        then do
        put (it, count - 1, maximum' - 1, predicate)
        -- rest <- findUntil2 it (count - 1) (maximum' - 1) predicate
        -- return (j:rest)
        return $ Just j
        else do
        put (it, count, maximum' - 1, predicate)
        -- rest <- findUntil2
        return Nothing
    else return Nothing


findUntil :: Rocks.Iterator -> Int -> Int -> (DBValue -> Bool) -> IO [DBValue]
findUntil it count maximum' predicate = if count > 0 && maximum' > 0
    then do
    print $ "count " ++ show count ++ " maximum' " ++ show maximum'
    Rocks.iterLast it
    rawTx <- Rocks.iterValue it
    Rocks.iterPrev it
    -- rawTx <- evalStateT getNLastValuesT it
    case rawTx of
      Nothing -> return []
      Just j  -> if predicate j
        then do
        rest <- findUntil it (count - 1) (maximum' - 1) predicate
        return (j:rest)
        else findUntil it count (maximum' - 1) predicate
    else return []


g12 = runResourceT $ do
  let path = superDBPath
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  it <- Rocks.iterOpen db Rocks.defaultReadOptions
  lift $ evalStateT (replicateM 3 findUntil2) (it, 3, 3, const True)
  -- lift $ findUntil2 it 3 9 (\_ -> True)


g11 = runResourceT $ do
  let path = superDBPath
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  it <- Rocks.iterOpen db Rocks.defaultReadOptions
  lift $ findUntil it 3 9 (const True)


-- getNFirstValues :: (MonadTrans t, MonadResource (t IO)) => Rocks.DB -> Int -> t IO [DBValue]
getNFirstValues :: Rocks.DB -> Int -> IO [DBValue]
getNFirstValues db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  lift $ evalStateT (replicateM n getNFirstValuesT) it


getNLastValues :: Rocks.DB -> Int -> IO [DBValue]
getNLastValues db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  lift $ evalStateT (replicateM n getNLastValuesT) it


getNLastValues2 :: Rocks.DB -> Int -> IO [(DBKey, DBValue)]
getNLastValues2 db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  _ <- replicateM (n - 1) $ Rocks.iterPrev it
  Rocks.iterItems it


-- getFirst :: (MonadResource (t IO), MonadTrans t) => Rocks.DB -> Int -> Int -> t IO [DBValue]
getFirst offset db count = drop offset <$> getNFirstValues db (offset + count )


-- getLast :: Int -> Rocks.DB ->  Int -> IO [(DBKey, DBValue)]
getLast offset db count = drop offset <$> getNLastValues db (offset + count )


findTransactionsForWallet1 :: Rocks.DB -> PublicKey -> Int -> Int -> IO [TransactionAPI]
findTransactionsForWallet1 db pubKey offset aCount = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  lift $ findTransactionsForWallet it pubKey aCount 10



superDBPath :: FilePath
superDBPath = "/tmp/haskell-rocksDB17"


test07 :: IO ()
test07 = do
  let path = superDBPath
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




go fun n = do
  let path = superDBPath
  db <- Rocks.open path def{Rocks.createIfMissing=True}
  -- m <- getNLastValues db n
  m <- fun db n
  print m
  Rocks.close db


g1 = go getNLastValues2 2
g2 = go (getFirst 0) 2
g3 = go (getLast 0) 3
g21 = go (getLast 0) 21



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
        | otherwise = error "There exist numbers which are not even or odd"
      mTx = map funTx txKV
  -- print txKV
  -- print mTx
  print $ map (txFilterByKey pubKey) mTx
  let ti t = TransactionInfo t (BC.pack "123") 2 False
      kv = map (\t -> (rHashT t, S.encode $ ti t)) mTx
      kvB = map (uncurry Rocks.Put) kv
      path = "/tmp/txInfo18"

  db <- Rocks.open path def{Rocks.createIfMissing=True}
  -- write tx to db
  Rocks.write db def{Rocks.sync = True} kvB

  -- read tx from db
  -- result <- mapM (Rocks.get db  Rocks.defaultReadOptions) (map fst kv)
  -- print $ map (\r -> (decodeRaw r) :: Maybe TransactionInfo) result

  -- find values in db
  what <- findTransactionsForWallet1 db pubKey 0 3
  print $ "we have found: " ++ show (length what) ++ "Transactions"
  Rocks.close db


instance Show Rocks.Iterator where
  show _ = "Iterator"


kvOffset :: IO (M.Map (PublicKey, Integer) Rocks.Iterator)
kvOffset = runResourceT $ do
  let n = 5
  keys <- lift $ replicateM n generateNewRandomAnonymousKeyPair
  let pubKeys = map (\(KeyPair pub _) -> pub ) keys
  let path = superDBPath
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=True}
  it <- Rocks.iterOpen db Rocks.defaultReadOptions
  let iter = replicate n it
  -- iter <- lift $ replicateM n (\i -> do {Rocks.iterPrev i; return i}) it
  let kv = zip (zip pubKeys [1..]) iter
  return $ M.fromList kv
