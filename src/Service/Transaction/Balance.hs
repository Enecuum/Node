{-# LANGUAGE PackageImports #-}
module Service.Transaction.Balance
  ( getBalanceForKey,
    addMicroblockToDB,
    runLedger) where

import Service.Types.PublicPrivateKeyPair
import Service.Types
import qualified Data.ByteString.Char8 as BC hiding (map)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified Data.HashTable.IO as H
import Control.Monad
import Service.Transaction.Storage (DBdescriptor(..),rHash, rValue, urValue, htK, unHtK, unHtA)
import Data.Default (def)


--type NBalanceTable = H.BasicHashTable PublicKey Amount
type BalanceTable = H.BasicHashTable BC.ByteString Amount



-- functions for CLI
getBalanceForKey :: DBdescriptor -> PublicKey -> IO Amount
getBalanceForKey db key = do
    Just v  <- Rocks.get (descrDBLedger db) Rocks.defaultReadOptions (htK key)
    return (unHtA v)




updateBalanceTable :: BalanceTable -> Transaction -> IO ()
updateBalanceTable ht (Transaction fromKey toKey am _ _ _) = do 
                                                    v1 <- H.lookup ht $ htK fromKey
                                                    v2 <- H.lookup ht $ htK toKey
                                                    case (v1,v2) of
                                                      (Nothing, _)       -> do return ()
                                                      (_, Nothing)       -> do return ()
                                                      (Just balanceFrom, Just balanceTo) -> do H.insert ht (htK fromKey) (balanceFrom - am)
                                                                                               H.insert ht (htK toKey) (balanceTo + am)

getTxsMicroblock :: Microblock -> [Transaction]
getTxsMicroblock (Microblock _ _ _ _ txs _) = txs

--something = do
getBalanceOfKeys :: Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys dbLedger tx = do
  let keys = concatMap getPubKeys tx
  let hashKeys = map htK keys
  balance  <- do
                let readKey k = (Rocks.get dbLedger Rocks.defaultReadOptions k)
                let toTuple k (Just b) = (,) k (unHtA b)
                mapM (\k -> liftM (toTuple k ) (readKey k)) hashKeys
  aBalanceTable <- H.fromList balance
  return aBalanceTable


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _) = [fromKey, toKey]


microblockIsExpected :: Microblock -> Bool
microblockIsExpected = undefined


run :: Microblock -> DBdescriptor -> Microblock -> IO ()
run m = if (not $ microblockIsExpected m) then error "We are exepecting another microblock" else runLedger

runLedger = undefined
addMicroblockToDB :: DBdescriptor -> Microblock -> IO ()
addMicroblockToDB (DBdescriptor dbTx dbMb dbLedger) m  =  do
    let txs = getTxsMicroblock m
    ht      <- getBalanceOfKeys dbLedger txs
    mapM_ (updateBalanceTable ht) txs
-- Write to db atomically
    writeMicroblockDB dbMb m
    writeTransactionDB dbTx txs
    writeLedgerDB dbLedger ht


writeMicroblockDB :: Rocks.DB -> Microblock -> IO ()
writeMicroblockDB db m = do
  let key = rHash m
      val  = rValue m
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ]


writeTransactionDB :: Rocks.DB -> [Transaction] -> IO ()
writeTransactionDB db tx = do
  let txKeyValue = map (\t -> (rHash t, rValue t) ) tx
  Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue)


writeLedgerDB ::  Rocks.DB -> BalanceTable -> IO ()
writeLedgerDB db bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> ((rHash . unHtK) k, rValue v)) ledgerKV
  Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue)
