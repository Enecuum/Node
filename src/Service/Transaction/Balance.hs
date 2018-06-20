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
import Service.Transaction.Storage (DBPoolDescriptor(..),DBPoolDescriptor(..),rHash, rValue, urValue, unHtK, unHtA)
import Data.Default (def)
import Data.Hashable
import Data.Pool

type BalanceTable = H.BasicHashTable PublicKey Amount
-- type BalanceTable = H.BasicHashTable BC.ByteString Amount



-- functions for CLI
getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO Amount
getBalanceForKey db key = do
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue key))
    Just v  <- withResource (poolLedger db) fun
    return (unHtA v)


instance Hashable PublicKey


updateBalanceTable :: BalanceTable -> Transaction -> IO ()
updateBalanceTable ht aTransaction = do
  case aTransaction of
    (WithSignature t _)        -> updateBalanceTable ht t
    (WithTime _ t)             -> updateBalanceTable ht t
    (RegisterPublicKey aKey aBalance) -> H.insert ht aKey aBalance
    (SendAmountFromKeyToKey fromKey toKey am) -> do v1 <- H.lookup ht $ fromKey
                                                    v2 <- H.lookup ht $ toKey
                                                    case (v1,v2) of
                                                      (Nothing, _)       -> do return ()
                                                      (_, Nothing)       -> do return ()
                                                      (Just balanceFrom, Just balanceTo) -> do H.insert ht fromKey (balanceFrom - am)
                                                                                               H.insert ht toKey (balanceTo + am)
    _ -> error "Unsupported type of transaction"

getTxsMicroblock :: Microblock -> [Transaction]
getTxsMicroblock (Microblock _ _ _ _ txs _) = txs


getBalanceOfKeys :: Pool Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys = undefined
-- getBalanceOfKeys db tx = do
--   let hashKeys = concatMap getPubKeys tx
--   let fun k = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue k))
--   let getBalanceByKey k = withResource db (fun k)
--   let toTuple k (Just b) = (,) k (unHtA b)
--   balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
--   aBalanceTable <- H.fromList balance
--   return aBalanceTable


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (WithSignature t _) = getPubKeys t
getPubKeys (WithTime _ t) = getPubKeys t
getPubKeys (RegisterPublicKey aKey _) = [aKey]
getPubKeys (SendAmountFromKeyToKey fromKey toKey _) = [fromKey, toKey]


microblockIsExpected :: Microblock -> Bool
microblockIsExpected = undefined


run :: Microblock -> DBPoolDescriptor -> Microblock -> IO ()
run m = if (not $ microblockIsExpected m) then error "We are expecting another microblock" else runLedger

-- run :: DBPoolDescriptor -> [Microblock] -> IO ()
-- run db m = do
--   let keys = _teamKeys

runLedger :: DBPoolDescriptor -> Microblock -> IO ()
runLedger (DBPoolDescriptor _ _ poolLedger) m = do
    let txs = getTxsMicroblock m
    ht      <- getBalanceOfKeys poolLedger txs
    mapM_ (updateBalanceTable ht) txs
    writeLedgerDB poolLedger ht


addMicroblockToDB :: DBPoolDescriptor -> Microblock -> IO ()
addMicroblockToDB (DBPoolDescriptor dbTx dbMb dbLedger) m  =  do
    let txs = getTxsMicroblock m
-- Write to db atomically
    writeMicroblockDB dbMb m
    writeTransactionDB dbTx txs



writeMicroblockDB :: Pool Rocks.DB -> Microblock -> IO ()
writeMicroblockDB db m = do
  let key = rHash m
      val  = rValue m
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource db fun

writeTransactionDB :: Pool Rocks.DB -> [Transaction] -> IO ()
writeTransactionDB db tx = do
  let txKeyValue = map (\t -> (rHash t, rValue t) ) tx
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue))
  withResource db fun

writeLedgerDB ::  Pool Rocks.DB -> BalanceTable -> IO ()
writeLedgerDB db bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (rValue k, rValue v)) ledgerKV
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue))
  withResource db fun
