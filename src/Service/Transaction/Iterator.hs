{-# LANGUAGE PackageImports #-}
module Service.Transaction.Iterator where
import           Control.Monad                      (replicateM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State          (StateT, evalStateT, get,
                                                     put)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Service.Types


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
