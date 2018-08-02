{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Iterator where
import           Control.Monad                       (replicateM, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State           (StateT, evalStateT, get,
                                                      put, runStateT)
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


getNLastValuesT :: StateT Rocks.Iterator IO (Maybe DBValue)
getNLastValuesT = do
  it <- get
  value <- Rocks.iterValue it
  Rocks.iterPrev it
  put it
  return value


findUntil :: Rocks.Iterator -> Int -> Int -> (DBValue -> Bool) -> IO [DBValue]
findUntil it count maximum' predicate = if count > 0 && maximum' > 0
    then do
    print $ "count " ++ show count ++ " maximum' " ++ show maximum'
    (rawTx, newIter) <- runStateT getNLastValuesT it
    case rawTx of
      Nothing -> print "Nothing" >> return []
      Just j  -> if predicate j
        then do
        rest <- findUntil newIter (count - 1) (maximum' - 1) predicate
        return (j:rest)
        else findUntil newIter count (maximum' - 1) predicate
    else print "out of bounds" >> return []


getNLastValues :: Rocks.DB -> Int -> IO [Maybe DBValue]
getNLastValues db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  lift $ evalStateT (replicateM n getNLastValuesT) it


getLast offset db count = drop offset <$> getNLastValues db (offset + count )

path1 :: FilePath
path1 = "/tmp/haskell-rocksDB18"

path2 :: FilePath
path2 = "/tmp/txInfo18"

superPubKey = read "KX8GHxYaejRMEmDrM7ZrbMudvKY57rwW1u4PZN9s3z1z" :: PublicKey

test07 :: IO ()
test07 = do
  let path = path1
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
  let path = path1
  db <- Rocks.open path def{Rocks.createIfMissing=True}
  -- m <- getNLastValues db n
  m <- fun db n
  print m
  Rocks.close db




g21 = go (getLast 0) 21
g3 = go (getLast 0) 12


testTxForWallet :: IO ()
testTxForWallet = do
  -- create tx
  tx <- genNTx 6
  -- print tx
  -- print $ length tx
  let pubKey = superPubKey
      txKV = zip [1..] tx
      funTx (k,t)
        | even k = t {_owner = pubKey }
        | odd k = t
        | otherwise = error "Exist numbers which are not even or odd!"
      mTx = map funTx txKV
  -- print txKV
  -- print mTx
      isGood = map (txFilterByKey pubKey) mTx
  print $ "Is key there: " ++ (show isGood)
  print $ length $ filter (==True) $ isGood
  let ti t = TransactionInfo t (BC.pack "123") 2 False
      kv = map (\t -> (rHashT t, S.encode $ ti t)) mTx
      kvB = map (uncurry Rocks.Put) kv
      path = path2

  db <- Rocks.open path def{Rocks.createIfMissing=True}
  -- write tx to db
  Rocks.write db def{Rocks.sync = True} kvB

  -- read tx from db
  result <- mapM (Rocks.get db  Rocks.defaultReadOptions) (map fst kv)
  print $ "In DB transactions: "
  print $ length $ map (\r -> (decodeRaw "TransactionInfo" r) :: Maybe TransactionInfo) result
  print "----------------------------------------"
  Rocks.close db





decodeTx :: [DBValue] -> IO ()
decodeTx txInfo = do
  let fun1 t = _tx (decodeThis "TransactionInfo" t :: TransactionInfo)
  let fun2 t = TransactionAPI { _tx = t, _txHash = rHashT t}
  let what = map (fun2 . fun1) txInfo
  print $ "we have found: " ++ show (length what) ++ "Transactions"
  print what




-- instance Show Rocks.Iterator where
--   show _ = "Iterator"


-- kvOffset :: IO (M.Map (PublicKey, Integer) Rocks.Iterator)
-- kvOffset = runResourceT $ do
--   let n = 5
--   keys <- lift $ replicateM n generateNewRandomAnonymousKeyPair
--   let pubKeys = map (\(KeyPair pub _) -> pub ) keys
--   let path = path1
--   (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=True}
--   it <- Rocks.iterOpen db Rocks.defaultReadOptions
--   let iter = replicate n it
--   -- iter <- lift $ replicateM n (\i -> do {Rocks.iterPrev i; return i}) it
--   let kv = zip (zip pubKeys [1..]) iter
--   return $ M.fromList kv


-- g2 = go (getFirst 0) 2
-- getNFirstValuesT :: StateT Rocks.Iterator IO DBValue
-- getNFirstValuesT = do
--   it <- get
--   Just v <- Rocks.iterValue it
--   Rocks.iterNext it
--   put it
--   return v

-- getNFirstValues :: Rocks.DB -> Int -> IO [DBValue]
-- getNFirstValues db n = runResourceT $ do
--   it    <- Rocks.iterOpen db Rocks.defaultReadOptions
--   Rocks.iterFirst it
--   lift $ evalStateT (replicateM n getNFirstValuesT) it


-- getFirst offset db count = drop offset <$> getNFirstValues db (offset + count )


-- getNLastValues2 :: Rocks.DB -> Int -> IO [(DBKey, DBValue)]
-- getNLastValues2 db n = runResourceT $ do
--   it    <- Rocks.iterOpen db Rocks.defaultReadOptions
--   Rocks.iterLast it
--   _ <- replicateM (n - 1) $ Rocks.iterPrev it
--   Rocks.iterItems it

-- g1 = go getNLastValues2 2
