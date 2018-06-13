{-# LANGUAGE PackageImports #-}
module Service.Transaction.Balance ( countBalance, runLedger ) where

--import Data.Monoid (mconcat)
import Service.System.Directory (getTransactionFilePath,getLedgerFilePath)
import Service.Types.PublicPrivateKeyPair
import Service.Types
import Node.FileDB.FileDB (readHashMsgFromFile)
--import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import  Data.ByteString.Char8 as BC hiding (map)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified Data.HashTable.IO as H
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Default (def)
import Service.Transaction.Microblock (genNMicroBlocks)
import Control.Monad.Trans.Resource

type HashOfMicroblock = BC.ByteString
type BalanceTable = H.BasicHashTable BC.ByteString Amount


getBalance :: PublicKey -> [Transaction] -> Amount
getBalance key transactions = sum $ map getAmount transactions
  where
    getAmount (WithSignature t _) = getAmount t
    getAmount (WithTime _ t) = getAmount t
    getAmount (SendAmountFromKeyToKey from to a)
      | from == key  = a *(-1)
      | to == key    = a
      | otherwise    = 0
    getAmount _ = 0

countBalance :: PublicKey -> IO Amount
countBalance key = getBalance key <$> (readTransactions =<< getTransactionFilePath)



readTransactions :: String -> IO [Transaction]
readTransactions fileName = do
    mblocks <-  readHashMsgFromFile fileName
    let ts =  [trs | (Microblock _ _ trs) <- mblocks]
    return $ mconcat ts

transformKey key = BC.pack . show $ key



writeMicroblockDB :: Microblock -> StateT Rocks.DB IO ()
writeMicroblockDB m = do
  let hash = SHA1.hash $ BC.pack $ show m
      val  = BC.pack $ show m
  db <- get
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put hash val ]
  put db


updateBalanceTable :: BalanceTable -> Transaction -> IO ()
updateBalanceTable ht tx = do
  case transaction tx of
    (RegisterPublicKey pbk b) -> do let key = BC.pack $ show pbk
                                    H.insert ht key b
    (WithTime _ change)       -> do let
                                        o    = owner change
                                        r    = receiver change
                                        am   = amount change
                                        key1 = BC.pack $ show o
                                        key2 = BC.pack $ show r
                                    v1 <- H.lookup ht key1
                                    v2 <- H.lookup ht key2
                                    case (v1,v2) of
                                      (Nothing, _)       -> do return ()
                                      (_, Nothing)       -> do return ()
                                      (Just b1, Just b2) -> do H.insert ht key1 (b1-am)
                                                               H.insert ht key2 (b1+am)


getTxsMicroblock (Microblock _ _ txs) = txs



getAllValues db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it


runLedger :: Rocks.DB -> Microblock -> IO ()
runLedger db m  = do
    ht      <- H.new
    execStateT (writeMicroblockDB m) db
    let txs = getTxsMicroblock m
    mapM_ (updateBalanceTable ht) txs
