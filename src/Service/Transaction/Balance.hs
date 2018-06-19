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
import Service.Transaction.Storage (DBdescriptor(..),rHash, rValue, urValue, unHtK, unHtA)
import Data.Default (def)
import Data.Hashable

type BalanceTable = H.BasicHashTable PublicKey Amount
-- type BalanceTable = H.BasicHashTable BC.ByteString Amount



-- functions for CLI
getBalanceForKey :: DBdescriptor -> PublicKey -> IO Amount
getBalanceForKey db key = do
    Just v  <- Rocks.get (descrDBLedger db) Rocks.defaultReadOptions (rValue key)
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

--something = do
getBalanceOfKeys :: Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys dbLedger tx = do
  let hashKeys = concatMap getPubKeys tx
  balance  <- do
                let getBalanceByKey k = (Rocks.get dbLedger Rocks.defaultReadOptions (rValue k))
                let toTuple k (Just b) = (,) k (unHtA b)
                mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  aBalanceTable <- H.fromList balance
  return aBalanceTable


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (WithSignature t _) = getPubKeys t
getPubKeys (WithTime _ t) = getPubKeys t
getPubKeys (RegisterPublicKey aKey _) = [aKey]
getPubKeys (SendAmountFromKeyToKey fromKey toKey _) = [fromKey, toKey]


microblockIsExpected :: Microblock -> Bool
microblockIsExpected = undefined


run :: Microblock -> DBdescriptor -> Microblock -> IO ()
run m = if (not $ microblockIsExpected m) then error "We are expecting another microblock" else runLedger

-- run :: DBdescriptor -> [Microblock] -> IO ()
-- run db m = do
--   let keys = _teamKeys

runLedger :: DBdescriptor -> Microblock -> IO ()
runLedger (DBdescriptor _ _ dbLedger) m = do
    let txs = getTxsMicroblock m
    ht      <- getBalanceOfKeys dbLedger txs
    mapM_ (updateBalanceTable ht) txs
    writeLedgerDB dbLedger ht


addMicroblockToDB :: DBdescriptor -> Microblock -> IO ()
addMicroblockToDB (DBdescriptor dbTx dbMb dbLedger) m  =  do
    let txs = getTxsMicroblock m
-- Write to db atomically
    writeMicroblockDB dbMb m
    writeTransactionDB dbTx txs



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
  -- let ledgerKeyValue = map (\(k,v)-> (rHash k, rValue v)) ledgerKV
  let ledgerKeyValue = map (\(k,v)-> (rValue k, rValue v)) ledgerKV
  Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue)
