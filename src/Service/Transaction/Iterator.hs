{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Iterator where
import           Control.Monad                      (replicateM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State          (StateT, evalStateT, get,
                                                     put, runStateT)
import           Data.Default                       (def)
import qualified Data.Map                           as M
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           System.IO.Unsafe


maxAttempt :: Int
maxAttempt = 15


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


getNLastValues :: Rocks.Iterator -> Int -> IO [Maybe DBValue]
getNLastValues it n = runResourceT $ do
  Rocks.iterLast it
  lift $ evalStateT (replicateM n getNLastValuesT) it


getLast :: Rocks.Iterator -> Int -> Int -> IO [Maybe DBValue]
getLast it offset count = drop offset <$> getNLastValues it (offset + count )


goo :: FilePath -> Int -> (DBValue -> Bool) -> IO [DBValue]
goo path n predicate = runResourceT $ do
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=False}
  it <- Rocks.iterOpen db Rocks.defaultReadOptions
  lift $ nLastValues it n predicate


nLastValues :: Rocks.Iterator -> Int -> (DBValue -> Bool) -> IO [DBValue]
nLastValues it n predicate = runResourceT $ do
  -- it <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  lift $ findUntil it n maxAttempt predicate


getLastIterator :: Rocks.DB -> IO Rocks.Iterator
getLastIterator db = runResourceT $ Rocks.iterOpen db Rocks.defaultReadOptions


getAllValues :: MonadUnliftIO m => Rocks.DB -> m [DBValue]
getAllValues db = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it


getAllItems :: MonadResource m => Rocks.DB -> m [(DBKey, DBValue)]
getAllItems db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterItems it


instance Show Rocks.Iterator where
  show _ = "Iterator"


kvOffset :: OffsetMap --(M.Map (PublicKey, Integer) Rocks.Iterator)
kvOffset = unsafePerformIO $ runResourceT $ do
  let n = 5
  keys <- lift $ replicateM n generateNewRandomAnonymousKeyPair
  let pubKeys = map (\(KeyPair pub _) -> pub ) keys
  let path = "" --path1
  (_, db) <- Rocks.openBracket path def{Rocks.createIfMissing=True}
  it <- Rocks.iterOpen db Rocks.defaultReadOptions
  let iter = replicate n it
  -- iter <- lift $ replicateM n (\i -> do {Rocks.iterPrev i; return i}) it
  let kv = zip (zip pubKeys [1..]) iter
  return $ M.fromList kv
