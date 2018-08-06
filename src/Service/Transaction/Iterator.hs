{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Iterator where
import           Control.Monad                      (replicateM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State          (StateT, get, put,
                                                     runStateT)
import qualified Data.ByteString.Internal           as BSI
import           Data.Pool
import qualified Data.Serialize                     as S
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Service.Transaction.Decode
import           Service.Types


maxAttempt :: Int
maxAttempt = 15


getNValues :: MonadResource m => Rocks.DB -> Int -> m [(BSI.ByteString, BSI.ByteString)]
getNValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  _ <- replicateM (n - 1) $ Rocks.iterPrev it
  Rocks.iterItems it


getNLastValuesT :: StateT Rocks.Iterator IO (Maybe DBValue)
getNLastValuesT = do
  it <- get
  value <- Rocks.iterValue it
  Rocks.iterPrev it
  put it
  return value


findUntil :: Rocks.Iterator -> Int -> Int -> (DBValue -> Bool) -> IO [(DBValue,Rocks.Iterator)]
findUntil it count maximum' predicate = if count > 0 && maximum' > 0
    then do
    print $ "count " ++ show count ++ " maximum' " ++ show maximum'
    (rawTx, newIter) <- runStateT getNLastValuesT it
    case rawTx of
      Nothing -> return []
      Just v  -> if predicate v
        then do
        rest <- findUntil newIter (count - 1) (maximum' - 1) predicate
        return ((v,newIter):rest)
        else findUntil newIter count (maximum' - 1) predicate
    else return []


getNLastValues :: Rocks.Iterator -> Int -> IO ([Maybe DBValue], Rocks.Iterator)
getNLastValues it n = runResourceT $ do
  Rocks.iterLast it
  lift $ runStateT (replicateM n getNLastValuesT) it


nLastValues :: Rocks.Iterator -> Int -> (DBValue -> Bool) -> IO ([DBValue], Rocks.Iterator)
nLastValues it n predicate = runResourceT $ do
  Rocks.iterLast it
  vs <- lift $ findUntil it n maxAttempt predicate
  let values = map fst vs
      lastIter = snd $ last vs
  return (values, lastIter)

getLastIterator :: Rocks.DB -> IO Rocks.Iterator
getLastIterator db = runResourceT $ Rocks.iterOpen db Rocks.defaultReadOptions


getAllValues :: Rocks.DB -> IO [DBValue]
getAllValues db = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it


getAllItems :: Rocks.DB -> IO [(BSI.ByteString, BSI.ByteString)]
getAllItems db = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterItems it


getAllKV ::  Pool Rocks.DB -> IO [(DBKey,DBValue)]
getAllKV db = withResource db getAllItems


getAllAndDecode :: Pool Rocks.DB -> (DBKey -> a) -> (DBValue -> b) -> IO [(a, b)]
getAllAndDecode db funcK funcV = do
  result <- withResource db getAllItems
  return $ map (\(k,v) -> (funcK k, funcV v)) result


getAllAndDecode2 :: (S.Serialize b, S.Serialize a) => Pool Rocks.DB -> DecodeType -> DecodeType -> IO [(a, b)]
getAllAndDecode2 db aType bType = getAllAndDecode db (decodeThis aType) (decodeThis bType)
